#include <algorithm>
#include <cmath>
#include <cstring>
#include <iostream>
#include <iterator>
#include <sstream>

#include "eval.hpp"
#include "hashtable.hpp"
#include "movepicker.hpp"
#include "perft.hpp"
#include "search.hpp"

int LMRTable[64][64];
int LateMovePruningCounts[2][11];
int FutilityPruningHistoryLimit[2];

// Thread related stuff
SearchWorker::~SearchWorker() {
  quit = true;
  startSearch();
  searchThread.join();
}

void SearchWorker::idleLoop() {
  while (true) {
    std::unique_lock<std::mutex> lock(mutex);
    searching = false;
    cv.notify_one(); // Check for search finished
    cv.wait(lock, [this] { return searching; });

    if (quit) {
      return;
    }

    lock.unlock();
    stop = false;
    searchPosition();
  }
}

void SearchWorker::startSearch() {
  mutex.lock();
  searching = true;
  mutex.unlock();
  cv.notify_one(); // Wake up idle loop thread
}

void SearchWorker::waitForSearchFinished() {
  std::unique_lock<std::mutex> lock(mutex);
  cv.wait(lock, [this] { return !searching; });
}

void SearchWorker::start(Position &pos, StateList &states, TimeControl &time,
                         Depth depth) {
  waitForSearchFinished();
  rootPos = pos;
  rootState = *pos.state();
  nodes = bestMoveChanges = 0;
  tm = time;
  maxDepth = depth;
  pvStability = 0;

  std::fill(pvLine, pvLine + MAX_DEPTH + 1, PVLine{});

  tm.startTime = getTimeMs();

  startSearch();
}

void SearchWorker::checkTime() {
  if (tm.idealStopTime == 0) {
    return;
  }

  bool passedIdeal = tm.elapsed() > tm.idealStopTime;
  bool passedMax = tm.elapsed() > tm.maxStopTime;

  pvStability =
      (pvLine[currentDepth].line[0] == pvLine[currentDepth - 1].line[0])
          ? std::min(10, pvStability + 1)
          : 0;

  if (finishSearch()) {
    stop = true;
  }
}

bool SearchWorker::finishSearch() {
  if (currentDepth <= 4)
    return false;

  Time elapsed = tm.elapsed();

  if (elapsed > tm.maxStopTime)
    return true;

  const double pvFactor = 1.20 - 0.04 * pvStability;

  const double scoreChange =
      bestValue[currentDepth] - bestValue[currentDepth - 3];

  const double scoreFactor = std::max(0.75, std::max(1.25, scoreChange * 0.05));

  return elapsed > tm.idealStopTime * scoreFactor * pvFactor;
}

std::string SearchWorker::pv(PVLine pvLine) {
  std::stringstream ss;

  Time time = tm.elapsed() + 1;
  int hashFull = TTHashFull();

  Value score = bestValue[currentDepth];

  ss << "info"
     << " depth " << currentDepth << " score " << score2Str(score) << " nodes "
     << nodes << " nps " << nodes * 1000 / time << " hashfull " << hashFull
     << " tbhits " << 0 << " time " << time << " pv";

  if (score > -VAL_MATE and score < -VAL_MATE_BOUND) {
    pvLine.length = score + VAL_MATE;
  } else if (score < VAL_MATE and score > VAL_MATE_BOUND) {
    pvLine.length = VAL_MATE - score;
  }

  for (int index = 0; index < pvLine.length; index++) {
    ss << " " << move2Str(Move(pvLine.line[index]));
  }

  return ss.str();
}

static void initLMR() {
  // Init Late Move Reductions Table (From Ethreal Chess Engine)
  for (int depth = 1; depth < 64; depth++)
    for (int played = 1; played < 64; played++)
      LMRTable[depth][played] = 0.7844 + log(depth) * log(played) / 2.4696;

  for (int depth = 1; depth <= 10; depth++) {
    LateMovePruningCounts[0][depth] = 2.0767 + 0.3743 * depth * depth;
    LateMovePruningCounts[1][depth] = 3.8733 + 0.7124 * depth * depth;
  }
}

static void updatePV(PVLine &parentPV, PVLine &childPV, Move &move) {
  parentPV.length = 1 + childPV.length;
  parentPV.line[0] = move;
  std::copy(childPV.line, childPV.line + childPV.length, parentPV.line + 1);
}

void SearchWorker::searchPosition() {
  // Iterative deepening
  TTUpdate();
  initLMR();
  iterativeDeepening();
  searching = false;
  waitForSearchFinished();
}

void SearchWorker::iterativeDeepening() {

  std::fill(bestValue, bestValue + MAX_DEPTH + 1, -VAL_INFINITE);

  for (currentDepth = 1; currentDepth <= maxDepth; currentDepth++) {
    aspirationWindow();
    if (stop) {
      break;
    }
    std::cout << pv(pvLine[currentDepth]) << std::endl;
    std::cout << bestMoveChanges << std::endl;
  }
  std::cout << "bestmove " << move2Str(pvLine[currentDepth - 1].line[0])
            << std::endl;
}

void SearchWorker::aspirationWindow() {
  Depth depth = currentDepth;
  Value alpha = -VAL_INFINITE, beta = VAL_INFINITE, delta = 10;

  Value best;
  PVLine current{};

  if (currentDepth >= 4) {
    alpha = std::max(int(-VAL_INFINITE), bestValue[currentDepth - 1] - delta);
    beta = std::min(int(VAL_INFINITE), bestValue[currentDepth - 1] + delta);
  }

  while (true) {
    best =
        search(rootPos, current, alpha, beta, std::max(int(depth), 1), false);

    if (stop) {
      break;
    }

    if (best > alpha and best < beta) {
      pvLine[currentDepth] = current;
      bestValue[currentDepth] = best;
      break;
    }

    if (best <= alpha) {
      beta = (alpha + beta) / 2;
      alpha = std::max(int(-VAL_INFINITE), alpha - delta);
      depth = currentDepth;
    }

    if (best >= beta) {
      beta = std::min(int(VAL_INFINITE), beta + delta);
      depth -= (std::abs(best) <= VAL_INFINITE / 2);
      pvLine[currentDepth] = current;
      bestValue[currentDepth] = best;
    }

    delta += delta / 2;
  }
}

void SearchWorker::updateKiller(Move killer, Depth depth) {
  if (ss.killer[depth][0] == killer) {
    ss.killer[depth][1] = ss.killer[depth][0];
    ss.killer[depth][0] = killer;
  }
}

void SearchWorker::updateHistory(const Position &pos, Move history,
                                 Depth depth) {
  ss.history[pos.getPiece(history.from())][history.to()] += depth * depth;
}

void SearchWorker::updateButterfly(const Position &pos, Move history,
                                   Depth depth) {
  ss.butterfly[pos.getPiece(history.from())][history.to()] += 1;
}

Value SearchWorker::search(Position &pos, PVLine &parentPV, Value alpha,
                           Value beta, Depth depth, bool cutNode) {

  const bool pvNode = (alpha != beta - 1);
  const bool rootNode = (ply == 0);
  const bool inCheck = pos.isInCheck();

  PVLine childPV{};
  bool doFullSearch = false, skipQuiets = false, improving = false;
  Value value, rAlpha, rBeta, evaluation, bestScore = -VAL_INFINITE,
                                          oldAlpha = alpha;
  Value ttHit = 0, ttValue = 0, ttEval = VAL_NONE;
  HashFlag ttFlag = HashNone;
  Depth R, ttDepth = 0;
  Count moveCount = 0;
  Move bestMove = Move::none(), move, ttMove = Move::none();
  BoardState st{};

  // Generate moves to check for draws
  if ((nodes & 2047) == 0)
    checkTime();

  // Check if the depth is 0, if so return the quiescence search
  if (depth <= 0 and !inCheck)
    return quiescence(pos, parentPV, alpha, beta); // Return quiescence

  // Increment nodes
  nodes++;

  // Early exit conditions
  if (!rootNode) {
    // From Ethreal Chess Engine (add variance to draw score to avoid
    // blindness to 3 fold lines)
    if (pos.isDraw(ply)) {
      return 1 - (nodes & 2);
    }
    // Check if reached max search depth
    if (ply >= MAX_DEPTH - 1) {
      return inCheck ? 0 : eval(pos);
    }

    rAlpha = std::max(int(alpha), -VAL_MATE + ply);
    rBeta = std::min(int(beta), VAL_MATE - ply - 1);

    if (rAlpha >= rBeta)
      return rAlpha;
  }

  depth = std::max(depth, Depth(0));

  if ((ttHit = TTProbe(pos.state()->key, ply, ttMove, ttValue, ttEval, ttDepth,
                       ttFlag))) {

    // Used cut node only if the search is at a greater depth
    // Do not return when in a pv node, unless we would hit a qsearch
    if (ttDepth >= depth and (depth == 0 || !pvNode) and
        (cutNode || ttValue <= alpha)) {
      if (ttFlag == HashExact || (ttFlag == HashBeta and ttValue >= beta) ||
          (ttFlag == HashAlpha and ttValue <= alpha)) {
        return ttValue;
      }

      // An entry with one depth less than the current depth would be accepted
      // if it appears that failing low will trigger a research
      if (!pvNode and ttDepth >= depth - 1 and (ttFlag & HashAlpha) and
          ttValue + 141 <= alpha) {
        return alpha;
      }
    }
  }

  // Static eval
  evaluation = inCheck ? 0 : eval(pos);

  // Improving
  improving = !inCheck && evaluation >= bestValue[currentDepth - 2];

  rBeta = std::min(beta + 100, VAL_MATE_BOUND - 1);

  if (!ttHit and !inCheck) {
    TTStore(pos.state()->key, ply, Move::none(), VAL_NONE, evaluation, Value(0),
            HashNone);
  }

  if (!pvNode and !inCheck and depth <= 8 and
      evaluation - 65 * std::max(0, (depth - improving)) >= beta) {
    return evaluation;
  }

  if (!pvNode and !inCheck and depth <= 4 and evaluation + 3488 <= alpha) {
    return evaluation;
  }

  // Get non pawn material
  Value nonPawnMaterial = pos.state()->nonPawnMaterial[pos.getSideToMove()].mg;

  // Null move pruning
  if (!pvNode and !inCheck and !cutNode and evaluation >= beta and
      depth >= 2 and nonPawnMaterial > 0 and
      (!ttHit || !(ttFlag & HashAlpha) || ttValue >= beta)) {

    ply++;

    R = 4 + depth / 5 + std::min(3, (evaluation - beta) / 191) +
        (pos.state()->captured == NO_PIECE);

    pos.makeNullMove(st);
    value = -search(pos, childPV, -beta, -beta + 1, depth - R, !cutNode);
    pos.unmakeNullMove();

    ply--;

    if (value >= beta) {
      return value > VAL_MATE_BOUND ? beta : value;
    }
  }

  // Internal iterative deepening
  if (cutNode and depth >= 7 and ttMove == Move::none()) {
    depth -= 1;
  }

  // Sort moves
  MovePicker mp(pos, ss, ply, depth, ttMove);

  while ((move = mp.pickNextMove(skipQuiets)) != Move::none()) {

    bool isCapture = pos.isCapture(move);
    // Late move pruning
    if (!skipQuiets and bestScore > -VAL_MATE_BOUND and depth <= 8 and
        moveCount >= LateMovePruningCounts[improving][depth]) {
      skipQuiets = true;
    }

    int hist = ss.history[pos.getPiece(move.from())][move.to()];
    // Quiet move pruning
    if (!skipQuiets and !isCapture and bestScore > -VAL_MATE_BOUND) {
      int lmrDepth = std::max(
          0,
          depth -
              LMRTable[std::min(63, int(depth))][std::min(63, int(moveCount))]);
      int fmpMargin = 77 + 52 * lmrDepth;

      if (!inCheck and evaluation + fmpMargin <= alpha and lmrDepth <= 8 and
          hist < FutilityPruningHistoryLimit[improving]) {
        skipQuiets = true;
      }

      if (!inCheck and lmrDepth <= 8 and
          evaluation + fmpMargin + 165 <= alpha) {
        skipQuiets = true;
      }
    }

    // Make move
    pos.makeMove(move, st);

    // Increment ply
    ply++;

    // Increment legal move counter
    moveCount++;

    if (depth > 2 and moveCount > 1 and !isCapture) {

      R = LMRTable[std::min(63, int(depth))][std::min(63, int(moveCount))];

      R += !pvNode + !improving;

      R += inCheck and pos.getPieceType(move.to()) == KING;

      R = std::min(depth - 1, std::max(int(R), 1));

      value = -search(pos, childPV, -alpha - 1, -alpha, depth - R, true);

      doFullSearch = value > alpha and R != 1;
    } else
      doFullSearch = !pvNode or moveCount > 1;

    // Full depth search on null window
    if (doFullSearch) {
      value = -search(pos, childPV, -alpha - 1, -alpha, depth - 1, !cutNode);
    }

    // Full depth search on full window for potential good moves
    if (pvNode and (moveCount == 1 || value > alpha)) {
      value = -search(pos, childPV, -beta, -alpha, depth - 1, false);
    }

    // Decrement ply
    ply--;
    // Unmake move
    pos.unmakeMove();

    if (stop)
      return 0;

    if (value > bestScore) {
      bestScore = value;
      bestMove = move;
      if (value > alpha) {
        // Update alpha
        alpha = value;

        updatePV(parentPV, childPV, move);

        // Update best move changes
        if (rootNode and moveCount > 1) {
          ++bestMoveChanges;
        }

        if (value >= beta) {
          break;
        } else {
          if (!pos.isCapture(move))
            updateButterfly(pos, bestMove, depth);
        }
      }
    }
  }

  if (bestScore >= beta and !pos.isCapture(bestMove)) {
    updateKiller(bestMove, depth);
    updateHistory(pos, bestMove, depth);
  }

  // Check if there are no moves, if so return the evaluation
  if (moveCount == 0) {
    // Check if in check
    return inCheck ? -VAL_MATE + ply : 0;
  }

  if (!rootNode) {
    ttFlag = bestScore >= beta      ? HashBeta
             : bestScore > oldAlpha ? HashExact
                                    : HashAlpha;
    bestMove = ttFlag == HashAlpha ? Move::none() : bestMove;
    TTStore(pos.state()->key, ply, bestMove, bestScore, evaluation, depth,
            ttFlag);
  }

  return bestScore;
}

Value SearchWorker::quiescence(Position &pos, PVLine &parentPV, Value alpha,
                               Value beta) {

  PVLine childPV{};

  Count moveCount = 0;
  Move move, ttMove = Move::none(), bestMove = Move::none();
  Value evaluation = 0, ttValue = 0, ttEval = VAL_NONE, value = 0,
        bestScore = -VAL_INFINITE, oldAlpha = alpha;
  bool ttHit;
  Depth ttDepth;
  HashFlag ttFlag;
  BoardState st{};

  const bool inCheck = pos.isInCheck();

  parentPV.length = 0;

  // Increment nodes
  nodes++;

  // Generate moves to check for draws
  if ((nodes & 2047) == 0)
    checkTime();

  // From Ethreal Chess Engine (add variance to draw score to avoid
  // blindness to 3 fold lines)
  if (pos.isDraw(ply)) {
    return 1 - (nodes & 2);
  }

  // Check if reached max search depth
  if (ply >= MAX_DEPTH - 1) {
    return eval(pos);
  }

  // Probe transposition table
  if ((ttHit = TTProbe(pos.state()->key, ply, ttMove, ttValue, ttEval, ttDepth,
                       ttFlag))) {
    if (ttFlag == HashExact || (ttFlag == HashBeta && ttValue >= beta) ||
        (ttFlag == HashAlpha && ttValue <= alpha))
      return ttValue;
  }

  // Get static evaluation
  evaluation = ttEval != VAL_NONE ? ttEval : eval(pos);

  // Save static evaluation
  if (!ttHit and !inCheck) {
    TTStore(pos.state()->key, ply, Move::none(), VAL_NONE, evaluation, 0,
            HashNone);
  }

  bestScore = evaluation;
  alpha = std::max(alpha, evaluation);
  if (alpha >= beta)
    return evaluation;

  // Step 6. Sort moves
  MovePicker mp(pos, ss, ply, 0, ttMove);

  while ((move = mp.pickNextMove(true)) != Move::none()) {
    // Make move
    pos.makeMove(move, st);
    // Increment ply
    ply++;
    // Increment move count
    moveCount++;
    // Full search
    value = -quiescence(pos, childPV, -beta, -alpha);
    // Decrement ply
    ply--;
    // Unmake move
    pos.unmakeMove();

    if (stop)
      return 0;

    if (value > bestScore) {
      bestScore = value;
      bestMove = move;
      if (value > alpha) {
        // Update alpha
        alpha = value;

        updatePV(parentPV, childPV, move);

        if (value >= beta) {
          break;
        }
      }
    }
  }

  ttFlag = bestScore >= beta      ? HashBeta
           : bestScore > oldAlpha ? HashExact
                                  : HashAlpha;
  TTStore(pos.state()->key, ply, bestMove, bestScore, evaluation, 0, ttFlag);

  return bestScore;
}

void SearchWorker::clear() {}