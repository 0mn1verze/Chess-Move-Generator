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
  pvStability = failHigh = failHighFirst = 0;

  std::fill(pvLine, pvLine + MAX_DEPTH + 1, PVLine{});

  tm.startTime = getTimeMs();

  startSearch();
}

void SearchWorker::checkTime() {
  if (tm.idealStopTime == 0) {
    return;
  }

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

  return elapsed > tm.idealStopTime * scoreFactor * pvFactor * 0.5;
}

void SearchWorker::tmUpdate() {
  pvStability =
      (pvLine[currentDepth].line[0] == pvLine[currentDepth - 1].line[0])
          ? std::min(10, pvStability + 1)
          : 0;
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

Value futility_margin(Depth d, bool improving) {
  return Value(217 * (d - improving));
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

  Value alpha = -VAL_INFINITE, beta = VAL_INFINITE;

  for (currentDepth = 1; currentDepth <= maxDepth; currentDepth++) {
    bestValue[currentDepth] =
        search(rootPos, pvLine[currentDepth], alpha, beta, currentDepth, false);
    if (stop) {
      break;
    }
    tmUpdate();
    if ((bestValue[currentDepth] <= alpha) ||
        (bestValue[currentDepth] >= beta)) {
      alpha = -VAL_INFINITE;
      beta = VAL_INFINITE;
      currentDepth--;
      continue;
    }

    alpha = bestValue[currentDepth] - 30;
    beta = bestValue[currentDepth] + 30;

    std::cout << pv(pvLine[currentDepth]) << std::endl;
    std::cout << "Best move changes: " << bestMoveChanges << std::endl;
    if (failHigh)
      std::cout << "Move ordering score: " << failHighFirst * 100 / failHigh
                << std::endl;
  }
  std::cout << "bestmove " << move2Str(pvLine[currentDepth - 1].line[0])
            << std::endl;
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

  const bool pvNode = (alpha != beta - 1) and pos.state()->move != Move::null();
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

  parentPV.length = 0;

  depth = std::max(depth, Depth(0));

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

  if ((ttHit = TTProbe(pos.state()->key, ply, ttMove, ttValue, ttEval, ttDepth,
                       ttFlag))) {

    // Used cut node only if the search is at a greater depth
    // Do not return when in a pv node, unless we would hit a qsearch
    if (!pvNode and ttDepth >= depth and ttValue != VAL_NONE and
        ttFlag & (ttValue >= beta ? HashBeta : HashAlpha)) {
      if (pos.state()->fiftyMove < 90) {
        return ttValue;
      }
    }
  }

  // Static eval
  evaluation = inCheck ? VAL_NONE : ttEval != VAL_NONE ? ttEval : eval(pos);

  if (ttHit and ttValue != VAL_NONE and
      (ttFlag & (ttValue > evaluation ? HashBeta : HashAlpha))) {
    evaluation = ttValue;
  }

  // Improving
  improving = !inCheck && evaluation > bestValue[currentDepth - 2];

  rBeta = std::min(beta + 100, VAL_MATE_BOUND - 1);

  if (!ttHit and !inCheck) {
    TTStore(pos.state()->key, ply, Move::none(), VAL_NONE, evaluation, Value(0),
            HashNone);
  }

  if (!pvNode and depth < 6 and
      evaluation - futility_margin(depth, improving) >= beta and
      !evaluation < 10000) {
    return evaluation;
  }

  // Get non pawn material
  Value nonPawnMaterial = pos.state()->nonPawnMaterial[pos.getSideToMove()];

  // Null move pruning
  if (!pvNode and !inCheck and pos.state()->move != Move::null() and
      evaluation >= beta and depth >= 2 and nonPawnMaterial > 0 and
      (!ttHit || !(ttFlag & HashAlpha) || ttValue >= beta) and
      beta >= -VAL_MATE_BOUND) {

    Depth R =
        (854 + 68 * depth) / 258 + std::min(int(evaluation - beta) / 192, 3);

    pos.makeNullMove(st);
    Value nullValue =
        -search(pos, childPV, -beta, -beta + 1, depth - R, !cutNode);
    pos.unmakeNullMove();

    if (nullValue >= beta) {
      if (nullValue >= VAL_MATE_BOUND)
        nullValue = beta;

      if (std::abs(beta) < 10000 and depth < 13) {
        return nullValue;
      }

      Value v = search(pos, childPV, beta - 1, beta, depth - 1, false);

      if (v >= beta)
        return nullValue;
    }
  }

  // Internal iterative deepening
  if (cutNode and depth >= 7 and ttMove == Move::none()) {
    depth -= 1;
  }

  // Sort moves
  MovePicker mp(pos, ss, ply, depth, ttMove);

  while ((move = mp.pickNextMove(skipQuiets)) != Move::none()) {

    // Increment legal move counter
    moveCount++;

    bool isCapture = pos.isCapture(move);
    // Late move pruning
    if (!rootNode and nonPawnMaterial and bestScore > VAL_MATE_BOUND)
      skipQuiets = moveCount >=
                   (improving ? 3 * depth * depth : (3 * depth * depth) / 2);

    // Make move
    pos.makeMove(move, st);

    // Increment ply
    ply++;

    if (depth > 2 and
        moveCount > 1 + rootNode + (rootNode && bestScore < alpha) and
        !rootNode and
        (!(isCapture or move.isPromotion()) || skipQuiets || cutNode)) {

      R = LMRTable[std::min(63, int(depth))][std::min(63, int(moveCount))];

      R += !pvNode + !improving;

      R += inCheck and pos.getPieceType(move.to()) == KING;

      R = std::min(depth - 1, std::max(int(R), 1));

      value = -search(pos, childPV, -alpha - 1, -alpha, depth - R, true);

      doFullSearch = value > alpha and R != 1;
    } else
      doFullSearch = !pvNode || moveCount > 1;

    if (doFullSearch) {
      value = -search(pos, childPV, -alpha - 1, -alpha, depth - 1, !cutNode);
    }

    if (pvNode and
        (moveCount == 1 || (value > alpha and (rootNode || value < beta)))) {
      value = -search(pos, childPV, -beta, -alpha, depth - 1, true);
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
          if (moveCount == 1)
            failHighFirst++;
          failHigh++;
          if (!pos.isCapture(move)) {
            updateKiller(bestMove, depth);
            updateHistory(pos, bestMove, depth);
          }
          break;
        } else {
          if (!pos.isCapture(move))
            updateButterfly(pos, bestMove, depth);
        }
      }
    }
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
        bestScore = -VAL_INFINITE, oldAlpha = alpha, futilityBase = 0,
        futilityValue = 0;
  bool ttHit;
  Depth ttDepth = 0;
  HashFlag ttFlag = HashNone;
  BoardState st{};

  const bool pvNode = (alpha != beta - 1);
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
    if (!pvNode and ttDepth >= 0 and ttValue != VAL_NONE and
        ttFlag & (ttValue >= beta ? HashBeta : HashAlpha)) {
      return ttValue;
    }
  }

  if (inCheck) {
    bestScore = futilityBase = -VAL_INFINITE;
  } else {
    if (ttHit) {
      if (ttEval == VAL_NONE) {
        bestScore = eval(pos);
      }

      if (ttValue != VAL_NONE and
          (ttFlag & (ttValue > bestScore ? HashBeta : HashAlpha))) {
        bestScore = ttValue;
      }
    } else
      bestScore = pos.state()->move != Move::null() ? eval(pos) : -VAL_INFINITE;

    if (bestScore >= beta) {
      if (!ttHit)
        TTStore(pos.state()->key, ply, Move::none(), VAL_NONE, evaluation, 0,
                HashNone);
      return bestScore;
    }

    if (pvNode and bestScore > alpha) {
      alpha = bestScore;
    }

    futilityBase = bestScore + 154;
  }

  // Step 6. Sort moves
  MovePicker mp(pos, ss, ply, 0, Move::none());

  while ((move = mp.pickNextMove(true)) != Move::none()) {
    // Increment move count
    moveCount++;
    if (!inCheck and futilityBase > -10000) {
      futilityValue = futilityBase + PieceValue[EG][pos.getPiece(move.to())];
      if (futilityValue <= alpha) {
        bestScore = std::max(bestScore, futilityValue);
        continue;
      }
    }
    // Make move
    pos.makeMove(move, st);
    // Increment ply
    ply++;
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

        // updatePV(parentPV, childPV, move);

        if (value >= beta) {
          break;
        }
      }
    }
  }

  if (inCheck and bestScore == -VAL_INFINITE) {
    return -VAL_MATE + ply;
  }

  if (std::abs(bestScore) < VAL_MATE_BOUND and bestScore >= beta) {
    bestScore = (3 * bestScore + beta) / 4;
  }

  ttFlag = bestScore >= beta      ? HashBeta
           : bestScore > oldAlpha ? HashExact
                                  : HashAlpha;
  TTStore(pos.state()->key, ply, bestMove, bestScore, evaluation, 0, ttFlag);

  return bestScore;
}

void SearchWorker::clear() {}