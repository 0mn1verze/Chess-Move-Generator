#include <iostream>

#include "defs.hpp"
#include "pawns.hpp"
#include "utils.hpp"

PawnTable pawnTable;
// Pawn penalties
constexpr Score Backward = _S(9, 24);
constexpr Score BlockedStorm = _S(82, 82);
constexpr Score Doubled = _S(11, 56);
constexpr Score Isolated = _S(5, 15);
constexpr Score WeakLever = _S(0, 56);
constexpr Score WeakUnopposed = _S(13, 27);

// Connected pawn bonus
constexpr int Connected[RANK_N] = {0, 7, 8, 12, 29, 48, 86};

// Strength of pawn shelter for our king by [distance from edge][rank].
// RANK_1 = 0 is used for files where we have no pawn, or pawn is behind our
// king.
constexpr Value ShelterStrength[int(FILE_N) / 2][RANK_N] = {
    {-6, 81, 93, 58, 39, 18, 25},
    {-43, 61, 35, -49, -29, -11, -63},
    {-10, 75, 23, -2, 32, 3, -45},
    {-39, -13, -29, -52, -48, -67, -166}};

// Danger of enemy pawns moving toward our king by [distance from edge][rank].
// RANK_1 = 0 is used for files where the enemy has no pawn, or their pawn
// is behind our king. Note that UnblockedStorm[0][1-2] accommodate opponent
// pawn on edge, likely blocked by our king.
constexpr Value UnblockedStorm[int(FILE_N) / 2][RANK_N] = {
    {85, -289, -166, 97, 50, 45, 50},
    {46, -25, 122, 45, 37, -10, 20},
    {-6, 51, 168, 34, -2, -22, -14},
    {-15, -11, 101, 4, 11, -15, -29}};

template <Colour us> Score evaluate(const Position &pos, PawnEntry *e) {
  constexpr Colour them = ~us;
  constexpr Direction up = pawnPush(us);

  Bitboard neighbours, stoppers, support, sameRank, opposed;
  Bitboard lever, leverPush, blocked;
  Square sq;
  bool backward, passed, doubled;
  Score score = _S(0, 0);
  // Used to loop over square
  Bitboard pawns = pos.getPiecesBB(us, PAWN);

  const Bitboard ourPawns = pos.getPiecesBB(us, PAWN);
  const Bitboard theirPawns = pos.getPiecesBB(them, PAWN);

  // Opponent pawns that double attack a square
  Bitboard doubleAttacked = pawnDoubleAttacksBB(them, theirPawns);

  e->passedPawnsBB[us] = 0;
  e->kingSq[us] = NO_SQ;
  e->attacks[us] = e->attackSpan[us] = pawnAttacksBB<us>(ourPawns);

  while (pawns) {
    sq = popLSB(pawns);
    Rank r = relativeRank(us, sq);

    // Pawns that are opposed by enemny pawns
    opposed = theirPawns & forwardFilesBB(us, sq);
    // Pawns that can blocked by enemy pawns
    blocked = theirPawns & (sq + up);
    // Pawns that are stopped by enemy pawns
    stoppers = theirPawns & passedPawnSpanBB(us, sq);
    // Pawns that can lever other pawns
    lever = theirPawns & pawnAttacksBB<us>(sq);
    // Pawns that can lever other pawns by pushing
    leverPush = theirPawns & pawnAttacksBB<us>(sq + up);
    // Pawns that are doubled
    doubled = ourPawns & (sq - up);
    // Neighbour pawns
    neighbours = ourPawns & adjacentFilesBB(sq);
    // Neighbour Pawns that are on the same rank
    sameRank = neighbours & rankBB(sq);
    // Neighbour Pawns that are supported by other pawns
    support = neighbours & rankBB(sq - up);

    // Backward pawn (When behind all pawns of same colour on the adjacent files
    // and cannot safely advanced)
    backward =
        !(neighbours & forwardRanksBB(them, sq + up)) && (leverPush | blocked);

    if (!backward and !blocked)
      e->attackSpan[us] |= pawnAttacksSpanBB(us, sq);

    // Passed pawn (When no enemy pawns can stop it from advancing)
    // (a) there is no stoppers except some levers
    // (b) the only stoppers are the leverPush, but we outnumber them
    // (c) there is only one front stopper which can be levered.
    passed = !(stoppers ^ lever) ||
             (!(stoppers ^ leverPush) and
              countBits(sameRank) >= countBits(leverPush)) ||
             (stoppers == blocked and r >= RANK_5 and
              (shift<up>(support) & ~(theirPawns | doubleAttacked)));

    if (passed)
      e->passedPawnsBB[us] |= sq;

    // Update score
    if (support | sameRank) {
      Value v = Connected[r] * (2 + bool(sameRank) - bool(opposed)) +
                21 * countBits(support);

      score += _S(v, v * (r - 2) / 4);
    } else if (!neighbours) {
      score -= Isolated + WeakUnopposed * !opposed;
    } else if (backward) {
      score -= Backward + WeakUnopposed * !opposed;
    }

    if (!support) {
      score -= Doubled * doubled + WeakLever * moreThanOne(lever);
    }
  }

  return score;
}

PawnEntry *probe(const Position &pos) {

  Key key = pos.state()->pawnKey;
  PawnEntry *e = pawnTable[key];

  if (e->key == key)
    return e;

  e->key = key;
  e->scores[WHITE] = evaluate<WHITE>(pos, e);
  e->scores[BLACK] = evaluate<BLACK>(pos, e);

  return e;
}

template <Colour us>
Score PawnEntry::evaluateShelter(const Position &pos, Square ksq) {
  constexpr Colour them = ~us;

  // Pawns in front of our king
  Bitboard b = pos.getPiecesBB(PAWN) & ~forwardRanksBB(them, ksq);
  // Our pawns in front
  Bitboard ourPawns = pos.getOccupiedBB(us) & b;
  // Their pawns in front
  Bitboard theirPawns = pos.getOccupiedBB(them) & b;

  Score bonus = _S(5, 5);

  File center = clamp(fileOf(ksq), FILE_B, FILE_G);

  for (File f = File(center - 1); f <= File(center + 1); ++f) {
    b = ourPawns & fileBB(f);
    Rank ourRank = b ? relativeRank(us, frontMostSq(them, b)) : RANK_1;

    b = theirPawns & fileBB(f);
    Rank theirRank = b ? relativeRank(us, frontMostSq(them, b)) : RANK_1;

    File d = fileFromEdge(f);
    bonus += _S(ShelterStrength[d][ourRank], 0);

    // Penalty for blocked pawn storms that are far too advanced
    if (ourRank and (ourRank == theirRank - 1))
      bonus -= BlockedStorm * int(theirRank == RANK_3);
    else
      bonus -= _S(UnblockedStorm[d][theirRank], 0);
  }

  return bonus;
}

template <Colour us> Score PawnEntry::doKingSafety(const Position &pos) {
  Square ksq = pos.kingSquare(us);
  kingSq[us] = ksq;
  castlingRights[us] = pos.castling(us);

  auto compare = [](Score a, Score b) { return a.mg < b.mg; };

  Score shelter = evaluateShelter<us>(pos, ksq);

  if (pos.state()->castling & KING_SIDE) {
    // Bonus for castling
    shelter = std::max(
        shelter, evaluateShelter<us>(pos, relativeSquare(us, G1)), compare);
  }

  if (pos.state()->castling & QUEEN_SIDE) {
    // Bonus for castling
    shelter = std::max(
        shelter, evaluateShelter<us>(pos, relativeSquare(us, C1)), compare);
  }

  Bitboard pawns = pos.getPiecesBB(us, PAWN);
  int minPawnDist = pawns ? 8 : 0;

  if (pawns & attacksBB<KING>(ksq)) {
    minPawnDist = 1;
  } else
    while (pawns)
      minPawnDist = std::min(minPawnDist, squareDist(ksq, popLSB(pawns)));

  return shelter - _S(0, 16 * minPawnDist);
}

template Score PawnEntry::doKingSafety<WHITE>(const Position &pos);
template Score PawnEntry::doKingSafety<BLACK>(const Position &pos);