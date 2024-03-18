#include <format>
#include <iostream>
#include <sstream>

#include "defs.hpp"
#include "eval.hpp"
#include "pawns.hpp"
#include "position.hpp"
#include "utils.hpp"

// Evaluation from Stockfish 11
class Evaluation {
public:
  Evaluation() = delete;
  explicit Evaluation(const Position &p) : pos(p), pe(probe(pos)){};

  template <Colour Us> void initialise();
  template <Colour Us, PieceType Pt> Score pieces();
  template <Colour Us> Score king() const;
  template <Colour Us> Score threats() const;
  template <Colour Us> Score passed() const;
  template <Colour Us> Score space() const;
  template <Colour Us> int imbalance() const;
  Score initiative(Score score) const;

  Value value();

  void trace();

private:
  const Position &pos;
  PawnEntry *pe;
  Bitboard mobilityArea[COLOUR_N];
  Score mobility[COLOUR_N] = {SCORE_ZERO, SCORE_ZERO};
  // Squares attacked by a piece
  Bitboard attackedBy[COLOUR_N][PIECE_TYPE_N]{};
  // Squares attacked by two pieces
  Bitboard attackedBy2[COLOUR_N]{};
  // Squares adjacent to the king + squares near the king
  Bitboard kingRing[COLOUR_N]{};
  // King attackers count
  int kingAttackersCount[COLOUR_N]{};
  // King attack weight
  int kingAttackersWeight[COLOUR_N]{};
  // King attacks count
  int kingAttacksCount[COLOUR_N]{};
};

constexpr Value EndgameLimit = 3915;
constexpr Value MidgameLimit = 15258;

// Threshold for lazy and space evaluation
constexpr Value LazyThreshold = Value(1400);
constexpr Value SpaceThreshold = Value(12222);

// KingAttackWeights[PieceType] contains king attack weights by piece type
constexpr int KingAttackWeights[PIECE_TYPE_N] = {0, 0, 81, 52, 44, 10};

// Penalties for enemy's safe checks
constexpr int QueenSafeCheck = 780;
constexpr int RookSafeCheck = 1080;
constexpr int BishopSafeCheck = 635;
constexpr int KnightSafeCheck = 790;

// MobilityBonus[PieceType-2][attacked] contains bonuses for middle and end
// game, indexed by piece type and number of attacked squares in the mobility
// area.
constexpr Score MobilityBonus[][32] = {
    {_S(-62, -81), _S(-53, -56), _S(-12, -30), _S(-4, -14), _S(3, 8),
     _S(13, 15), // Knights
     _S(22, 23), _S(28, 27), _S(33, 33)},
    {_S(-48, -59), _S(-20, -23), _S(16, -3), _S(26, 13), _S(38, 24),
     _S(51, 42), // Bishops
     _S(55, 54), _S(63, 57), _S(63, 65), _S(68, 73), _S(81, 78), _S(81, 86),
     _S(91, 88), _S(98, 97)},
    {_S(-58, -76), _S(-27, -18), _S(-15, 28), _S(-10, 55), _S(-5, 69),
     _S(-2, 82), // Rooks
     _S(9, 112), _S(16, 118), _S(30, 132), _S(29, 142), _S(32, 155),
     _S(38, 165), _S(46, 166), _S(48, 169), _S(58, 171)},
    {_S(-39, -36), _S(-21, -15), _S(3, 8),     _S(3, 18),    _S(14, 34),
     _S(22, 54), // Queens
     _S(28, 61),   _S(41, 73),   _S(43, 79),   _S(48, 92),   _S(56, 94),
     _S(60, 104),  _S(60, 113),  _S(66, 120),  _S(67, 123),  _S(70, 126),
     _S(71, 133),  _S(73, 136),  _S(79, 140),  _S(88, 143),  _S(88, 148),
     _S(99, 166),  _S(102, 170), _S(102, 175), _S(106, 184), _S(109, 191),
     _S(113, 206), _S(116, 212)}};

// RookOnFile[semiopen/open] contains bonuses for each rook when there is
// no (friendly) pawn on the rook file.
constexpr Score RookOnFile[] = {_S(21, 4), _S(47, 25)};

// ThreatByMinor/ByRook[attacked PieceType] contains bonuses according to
// which piece type attacks which one. Attacks on lesser pieces which are
// pawn-defended are not considered.
constexpr Score ThreatByMinor[PIECE_TYPE_N] = {
    _S(0, 0), _S(6, 32), _S(59, 41), _S(79, 56), _S(90, 119), _S(79, 161)};

constexpr Score ThreatByRook[PIECE_TYPE_N] = {
    _S(0, 0), _S(3, 44), _S(38, 71), _S(38, 61), _S(0, 38), _S(51, 38)};

// PassedRank[Rank] contains a bonus according to the rank of a passed pawn
constexpr Score PassedRank[RANK_N] = {_S(0, 0),    _S(10, 28), _S(17, 33),
                                      _S(15, 41),  _S(62, 72), _S(168, 177),
                                      _S(276, 260)};

// Assorted bonuses and penalties
constexpr Score BishopPawns = _S(3, 7);
constexpr Score CorneredBishop = _S(50, 50);
constexpr Score FlankAttacks = _S(8, 0);
constexpr Score Hanging = _S(69, 36);
constexpr Score KingProtector = _S(7, 8);
constexpr Score KnightOnQueen = _S(16, 12);
constexpr Score LongDiagonalBishop = _S(45, 0);
constexpr Score MinorBehindPawn = _S(18, 3);
constexpr Score Outpost = _S(30, 21);
constexpr Score PassedFile = _S(11, 8);
constexpr Score PawnlessFlank = _S(17, 95);
constexpr Score RestrictedPiece = _S(7, 7);
constexpr Score ReachableOutpost = _S(32, 10);
constexpr Score RookOnQueenFile = _S(7, 6);
constexpr Score SliderOnQueen = _S(59, 18);
constexpr Score ThreatByKing = _S(24, 89);
constexpr Score ThreatByPawnPush = _S(48, 39);
constexpr Score ThreatBySafePawn = _S(173, 94);
constexpr Score TrappedRook = _S(52, 10);
constexpr Score WeakQueen = _S(49, 15);

// Polynomial material imbalance parameters

constexpr int QuadraticOurs[][PIECE_TYPE_N] = {
    //            OUR PIECES
    // pair pawn knight bishop rook queen
    {1438},                        // Bishop pair
    {40, 38},                      // Pawn
    {32, 255, -62},                // Knight      OUR PIECES
    {0, 104, 4, 0},                // Bishop
    {-26, -2, 47, 105, -208},      // Rook
    {-189, 24, 117, 133, -134, -6} // Queen
};

constexpr int QuadraticTheirs[][PIECE_TYPE_N] = {
    //           THEIR PIECES
    // pair pawn knight bishop rook queen
    {0},                        // Bishop pair
    {36, 0},                    // Pawn
    {9, 63, 0},                 // Knight      OUR PIECES
    {59, 65, 42, 0},            // Bishop
    {46, 39, 24, -24, 0},       // Rook
    {97, 100, -42, 137, 268, 0} // Queen
};

template <Colour Us> void Evaluation::initialise() {
  constexpr Colour Them = ~Us;
  constexpr Direction Up = pawnPush(Us);
  constexpr Direction Down = pawnPush(Them);
  constexpr Bitboard LowRanks = lowRanks(Us);

  const Square ksq = pos.kingSquare(Us);
  const BoardState *st = pos.state();

  Bitboard doubleAttackedByPawn =
      pawnDoubleAttacksBB(Us, pos.getPiecesBB(Us, PAWN));

  Bitboard blocked =
      pos.getPiecesBB(Us, PAWN) & (shift<Down>(pos.getOccupiedBB()) | LowRanks);

  // Square occupied by blocked pawns, king, queen, blockers to attacks on king
  // and squares controlled by enemy pawns are excluded from the mobility area
  mobilityArea[Us] = ~(blocked | pos.getPiecesBB(Us, KING, QUEEN) |
                       st->pinned[Us] | pe->pawnAttacks(Them));

  // Initialise attackedBy and attackedBy2 for kings and pawns
  attackedBy[Us][KING] = attacksBB<KING>(ksq);
  attackedBy[Us][PAWN] = pe->pawnAttacks(Us);
  attackedBy[Us][ALL_PIECES] = attackedBy[Us][KING] | attackedBy[Us][PAWN];
  attackedBy2[Us] =
      doubleAttackedByPawn | (attackedBy[Us][KING] & attackedBy[Us][PAWN]);

  // Init king safety table
  Square _s = toSquare(clamp(fileOf(ksq), FILE_B, FILE_G),
                       clamp(rankOf(ksq), RANK_2, RANK_7));
  kingRing[Us] = attacksBB<KING>(_s) | _s;
  kingAttackersCount[Them] = countBits(kingRing[Us] & pe->pawnAttacks(Them));
  kingAttacksCount[Them] = kingAttackersWeight[Them] = 0;

  // Remove from kingRing squares defended by two of our pawns
  kingRing[Us] &= ~doubleAttackedByPawn;
}

template <Colour Us, PieceType Pt> Score Evaluation::pieces() {
  constexpr Colour Them = ~Us;
  constexpr Direction Down = pawnPush(Them);
  constexpr Bitboard OutpostRanks = outPostRanks(Us);

  Bitboard pieces = pos.getPiecesBB(Us, Pt);
  BoardState *st = pos.state();
  const Square ksq = pos.kingSquare(Us);

  Bitboard b, bb;
  Square sq;
  Score score = SCORE_ZERO;

  attackedBy[Us][Pt] = 0;

  while (pieces) {
    sq = popLSB(pieces);
    // Get the piece attacks
    b = Pt == BISHOP ? attacksBB<BISHOP>(sq, pos.getOccupiedBB() ^
                                                 pos.getPiecesBB(QUEEN))
        : Pt == ROOK
            ? attacksBB<ROOK>(sq, pos.getOccupiedBB() ^ pos.getPiecesBB(QUEEN) ^
                                      pos.getPiecesBB(ROOK))
            : attacksBB<Pt>(sq, pos.getOccupiedBB());

    // Restrict attacked squares if pinned
    if (st->pinned[Us] & sq)
      b &= lineBB[ksq][sq];

    // Update attacked by us and attacked by 2 arrays
    attackedBy2[Us] |= attackedBy[Us][ALL_PIECES] & b;
    attackedBy[Us][Pt] |= b;
    attackedBy[Us][ALL_PIECES] |= b;

    // Update king attack count and weight
    if (b & kingRing[Them]) {
      kingAttackersCount[Us]++;
      kingAttackersWeight[Us] += KingAttackWeights[Pt];
      kingAttacksCount[Us] += countBits(b & attackedBy[Them][KING]);
    }

    int mob = countBits(b & mobilityArea[Us]);

    mobility[Us] += MobilityBonus[Pt - 2][mob];

    if (Pt == BISHOP || Pt == KNIGHT) {
      // Bonus if piece is on outpost square or can reach one
      bb = OutpostRanks & attackedBy[Us][PAWN] & ~pe->pawnAttacksSpan(Them);
      if (bb & sq)
        score += Outpost * (Pt == KNIGHT ? 2 : 1);
      else if (Pt == KNIGHT && bb & b & ~pos.getOccupiedBB(Us))
        score += ReachableOutpost;

      if (shift<Down>(pos.getPiecesBB(PAWN)) & sq)
        score += MinorBehindPawn;

      score -= KingProtector * squareDist(sq, ksq);

      if (Pt == BISHOP) {
        // Penalty according to number of pawns on the same color square as the
        // bishop, bigger when the center files are blocked with pawns.
        // Blocked pawns
        Bitboard blocked =
            pos.getPiecesBB(Us, PAWN) & shift<Down>(pos.getOccupiedBB());

        score -= BishopPawns *
                 countBits(pos.getPiecesBB(Us, PAWN) & sameColourSquares(sq)) *
                 (1 + countBits(blocked & CenterFiles));
        // Bonus for bishop on a long diagonal which can "see"
        // both center squares
        if (moreThanOne(attacksBB<BISHOP>(sq, pos.getPiecesBB(PAWN)) & Center))
          score += LongDiagonalBishop;
      }
    }

    if (Pt == ROOK) {
      // Penalty for rooks on queen file
      if (fileBB(sq) & pos.getPiecesBB(QUEEN))
        score += RookOnQueenFile;

      // Bonus for rooks attacking the enemy queen
      if (pos.isOnSemiOpenFile(Us, sq))
        score += RookOnFile[pos.isOnSemiOpenFile(Them, sq)];

      // Penality when trapped by the king, even more if the king cannot castle
      else if (mob <= 3) {
        File kf = fileOf(ksq);
        if ((kf < FILE_E) == (fileOf(sq) < kf))
          score -= TrappedRook * (1 + !pos.castling(Us));
      }
    }

    if (Pt == QUEEN) {
      // Penalty if any relative pin or discovered attacks against the queen
      Bitboard queenPinners;
      if (pos.getSliderBlockers(pos.getPiecesBB(Them, BISHOP, ROOK), sq,
                                queenPinners))
        score -= WeakQueen;
    }
  }

  return score;
}

template <Colour Us> Score Evaluation::king() const {
  constexpr Colour Them = ~Us;
  constexpr Bitboard Camp =
      (Us == WHITE ? FULLBB ^ rankBB(RANK_6) ^ rankBB(RANK_7) ^ rankBB(RANK_8)
                   : FULLBB ^ rankBB(RANK_1) ^ rankBB(RANK_2) ^ rankBB(RANK_3));

  Bitboard weak, b1, b2, b3, safe, unsafeChecks = 0;
  Bitboard rookChecks, queenChecks, bishopChecks, knightChecks;
  int kingDanger = 0;
  const Square ksq = pos.kingSquare(Us);

  // Init the score with king shelter and enemy pawn storm
  Score score = pe->kingSafety<Us>(pos);

  // Attacked squares defended at most once by queen or king
  weak = attackedBy[Them][ALL_PIECES] & ~attackedBy2[Us] &
         (~attackedBy[Us][ALL_PIECES] | attackedBy[Us][KING] |
          attackedBy[Us][QUEEN]);

  // Analyse the safe enemy's checks and queen pins which are possible on the
  // next move
  safe = ~pos.getOccupiedBB(Them);
  safe &= ~attackedBy[Us][ALL_PIECES] | (weak & attackedBy2[Them]);

  b1 = attacksBB<BISHOP>(ksq, pos.getOccupiedBB() ^ pos.getPiecesBB(QUEEN));
  b2 = attacksBB<ROOK>(ksq, pos.getOccupiedBB() ^ pos.getPiecesBB(QUEEN));

  rookChecks = b2 & safe & attackedBy[Them][ROOK];
  if (rookChecks)
    kingDanger += RookSafeCheck;
  else
    unsafeChecks |= b2 & attackedBy[Them][ROOK];

  // Enemy queen safe checks: we count them only if they are from squares from
  // which we can't give a rook check, because rook checks are more valuable.
  queenChecks = (b1 | b2) & safe & attackedBy[Them][QUEEN] &
                ~attackedBy[Us][QUEEN] & ~rookChecks;

  if (queenChecks)
    kingDanger += QueenSafeCheck;

  // Enemy bishops checks: we count them only if they are from squares from
  // which we can't give a queen check, because queen checks are more valuable.
  bishopChecks = b1 & safe & attackedBy[Them][BISHOP] & ~queenChecks;

  if (bishopChecks)
    kingDanger += BishopSafeCheck;
  else
    unsafeChecks |= b1 & attackedBy[Them][BISHOP];

  // Enemy knights checks
  knightChecks = attacksBB<KNIGHT>(ksq) & attackedBy[Them][KNIGHT];

  if (knightChecks & safe)
    kingDanger += KnightSafeCheck;
  else
    unsafeChecks |= knightChecks;

  // Find the squares that opponent attacks in our king flank, the squares
  // which they attack twice in that flank, and the squares that we defend.
  b1 = attackedBy[Them][ALL_PIECES] & KingFlank[fileOf(ksq)] & Camp;
  b2 = b1 & attackedBy2[Them];
  b3 = attackedBy[Us][ALL_PIECES] & KingFlank[fileOf(ksq)] & Camp;

  int kingFlankAttack = countBits(b1) + countBits(b2);
  int kingFlankDefense = countBits(b3);

  kingDanger +=
      kingAttackersCount[Them] * kingAttackersWeight[Them] +
      185 * countBits(kingRing[Us] & weak) + 148 * countBits(unsafeChecks) +
      98 * countBits(pos.state()->pinned[Us]) + 69 * kingAttacksCount[Them] +
      3 * kingFlankAttack * kingFlankAttack / 8 +
      (mobility[Them] - mobility[Us]).mg -
      873 * !pos.getPieceCount(toPiece(Them, QUEEN)) -
      100 * bool(attackedBy[Us][KNIGHT] & attackedBy[Us][KING]) -
      6 * score.mg / 8 - 4 * kingFlankDefense + 37;

  if (kingDanger > 100)
    score -= _S(kingDanger * kingDanger / 4096, kingDanger / 16);

  if (!(pos.getPiecesBB(PAWN) & KingFlank[fileOf(ksq)]))
    score -= PawnlessFlank;

  score -= FlankAttacks * kingFlankAttack;

  return score;
}

template <Colour Us> Score Evaluation::threats() const {
  constexpr Colour Them = ~Us;
  constexpr Direction Up = pawnPush(Us);
  constexpr Bitboard TRank3BB = (Us == WHITE ? rankBB(RANK_3) : rankBB(RANK_6));

  Bitboard b, weak, defended, nonPawnEnemies, stronglyProtected, safe;
  Score score = SCORE_ZERO;

  // Non-pawn enemies
  nonPawnEnemies = pos.getOccupiedBB(Them) & ~pos.getPiecesBB(PAWN);

  // Squares strongly protected by the enemy, either because they defend the
  // square with a pawn, or because they defend the square twice and we don't.
  stronglyProtected =
      attackedBy[Them][PAWN] | (attackedBy2[Them] & ~attackedBy2[Us]);

  // Non-pawn enemies which are strongly protected
  defended = nonPawnEnemies & stronglyProtected;

  // Enemies which are not strongly protected and are under attack
  weak =
      pos.getOccupiedBB(Them) & ~stronglyProtected & attackedBy[Us][ALL_PIECES];

  if (defended | weak) {
    b = (defended | weak) & (attackedBy[Us][KNIGHT] | attackedBy[Us][BISHOP]);
    while (b)
      score += ThreatByMinor[pos.getPieceType(popLSB(b))];

    b = weak & attackedBy[Us][ROOK];
    while (b)
      score += ThreatByRook[pos.getPieceType(popLSB(b))];

    if (weak & attackedBy[Us][KING])
      score += ThreatByKing;

    b = ~attackedBy[Them][ALL_PIECES] | (nonPawnEnemies & attackedBy2[Us]);
    score += Hanging * countBits(weak & b);
  }

  // Bonus for restricting their piece moves
  b = attackedBy[Them][ALL_PIECES] & ~stronglyProtected &
      attackedBy[Us][ALL_PIECES];

  score += RestrictedPiece * countBits(b);

  // Protected or unattacked squares
  safe = ~attackedBy[Them][ALL_PIECES] | attackedBy[Us][ALL_PIECES];

  // Bonus for attacking enemy pieces with our relatively safe pawns
  b = pos.getPiecesBB(Us, PAWN) & safe;
  b = pawnAttacksBB<Us>(b) & nonPawnEnemies;
  score += ThreatBySafePawn * countBits(b);

  // Find squares where our pawns can push on the next move
  b = shift<Up>(pos.getPiecesBB(Us, PAWN)) & ~pos.getOccupiedBB();
  b |= shift<Up>(b & TRank3BB) & ~pos.getOccupiedBB();

  // Keep only the squares which are relatively safe
  b &= ~attackedBy[Them][PAWN] & safe;

  // Bonus for safe pawn threats on the next move
  b = pawnAttacksBB<Us>(b) & nonPawnEnemies;
  score += ThreatByPawnPush * countBits(b);

  // Bonus for threats on the next moves against enemy queen
  if (pos.getPieceCount(toPiece(Them, QUEEN)) == 1) {
    Square s = getLSB(pos.getPiecesBB(Them, QUEEN));
    safe = mobilityArea[Us] & ~stronglyProtected;

    b = attackedBy[Us][KNIGHT] & attacksBB<KNIGHT>(s);

    score += KnightOnQueen * countBits(b & safe);

    b = (attackedBy[Us][BISHOP] & attacksBB<BISHOP>(s, pos.getOccupiedBB())) |
        (attackedBy[Us][ROOK] & attacksBB<ROOK>(s, pos.getOccupiedBB()));

    score += SliderOnQueen * countBits(b & safe & attackedBy2[Us]);
  }

  return score;
}

template <Colour Us> Score Evaluation::passed() const {
  constexpr Colour Them = ~Us;
  constexpr Direction Up = pawnPush(Us);

  auto kingProximity = [&](Colour c, Square sq) {
    return std::min(squareDist(pos.kingSquare(c), sq), 5);
  };

  Bitboard b, bb, squaresToQueen, unsafeSquares;
  Score score = SCORE_ZERO;

  b = pe->passedPawns(Us);

  while (b) {
    Square sq = popLSB(b);

    Rank r = relativeRank(Us, sq);

    Score bonus = PassedRank[r];

    if (r > RANK_3) {
      int w = 5 * r - 13;
      Square blockSq = sq + Up;

      // Adjust bonus based on the king's proximity
      bonus += _S(0, ((kingProximity(Them, blockSq) * 19) / 4 -
                      kingProximity(Us, blockSq) * 2) *
                         w);

      // If blockSq is not the queening square then consider also a second push
      if (r != RANK_7)
        bonus -= _S(0, kingProximity(Us, blockSq + Up) * w);

      // If the pawn is free to advance, then increase the bonus
      if (!(blockSq & pos.getOccupiedBB())) {
        squaresToQueen = forwardFilesBB(Us, sq);
        unsafeSquares = passedPawnSpanBB(Us, sq);

        bb = forwardFilesBB(Them, sq) & pos.getPiecesBB(ROOK, QUEEN);

        if (!(pos.getOccupiedBB(Them) & bb))
          unsafeSquares &= attackedBy[Them][ALL_PIECES];

        // If there are no enemy attacks on passed pawn span, assign a big
        // bonus. Otherwise assign a smaller bonus if the path to queen is not
        // attacked and even smaller bonus if it is attacked but block square is
        // not.
        int k = !unsafeSquares                      ? 35
                : !(unsafeSquares & squaresToQueen) ? 20
                : !(unsafeSquares & blockSq)        ? 9
                                                    : 0;

        // Assign a larger bonus if the block square is defended
        if ((pos.getOccupiedBB(Us) & bb) ||
            (attackedBy[Us][ALL_PIECES] & blockSq))
          k += 5;

        bonus += _S(k * w, k * w);
      }
    }

    // Scale down bonus for candidate passers which need more than one
    // pawn push to become passed, or have a pawn in front of them.
    if ((pos.getPiecesBB(Them, PAWN) & passedPawnSpanBB(Us, sq + Up)) ||
        (pos.getPiecesBB(PAWN) & (sq + Up)))
      bonus = bonus / 2;

    score += bonus - PassedFile * fileFromEdge(fileOf(sq));
  }

  return score;
}

template <Colour Us> Score Evaluation::space() const {
  if (pos.nonPawnMaterial() < SpaceThreshold)
    return SCORE_ZERO;

  constexpr Colour Them = ~Us;
  constexpr Direction Down = -pawnPush(Us);
  constexpr Bitboard SpaceMask =
      Us == WHITE
          ? CenterFiles & (rankBB(RANK_2) | rankBB(RANK_3) | rankBB(RANK_4))
          : CenterFiles & (rankBB(RANK_7) | rankBB(RANK_6) | rankBB(RANK_5));

  // Find the available squares for our pieces inside the area defined by
  // SpaceMask
  Bitboard safe =
      SpaceMask & ~pos.getPiecesBB(Us, PAWN) & ~attackedBy[Them][PAWN];

  // Find all squares which are at most three squares behind some friendly pawn
  Bitboard behind = pos.getPiecesBB(Us, PAWN);
  behind |= shift<Down>(behind);
  behind |= shift<Direction(Down + Down)>(behind);

  int bonus = countBits(safe) +
              countBits(behind & safe & ~attackedBy[Them][ALL_PIECES]);
  int weight = pos.getPieceCount(toPiece(Us, ALL_PIECES)) - 1;
  Score score = _S(bonus * weight * weight / 16, 0);

  return score;
}

Score Evaluation::initiative(Score score) const {
  Value mg = score.mg;
  Value eg = score.eg;

  int outflanking = fileDist(pos.kingSquare(WHITE), pos.kingSquare(BLACK)) -
                    rankDist(pos.kingSquare(WHITE), pos.kingSquare(BLACK));

  bool infiltration = rankOf(pos.kingSquare(WHITE)) > RANK_4 ||
                      rankOf(pos.kingSquare(BLACK)) < RANK_5;

  bool pawnsOnBothFlanks =
      (pos.getPiecesBB(PAWN) & QueenSide) && (pos.getPiecesBB(PAWN) & KingSide);

  bool almostUnwinnable =
      !pe->passedPawnsCount() && outflanking < 0 && !pawnsOnBothFlanks;

  // Compute the initiative bonus for the attacking side
  int complexity = 9 * pe->passedPawnsCount() +
                   11 * (pos.getPieceCount(wP) + pos.getPieceCount(bP)) +
                   9 * outflanking + 12 * infiltration +
                   21 * pawnsOnBothFlanks + 51 * !pos.nonPawnMaterial() -
                   43 * almostUnwinnable - 100;

  // Now apply the bonus: note that we find the attacking side by extracting the
  // sign of the midgame or endgame values, and that we carefully cap the bonus
  // so that the midgame and endgame scores do not change sign after the bonus.
  int u =
      ((mg > 0) - (mg < 0)) * std::max(std::min(complexity + 50, 0), -abs(mg));
  int v = ((eg > 0) - (eg < 0)) * std::max(complexity, -abs(eg));

  return _S(u, v);
}

Value Evaluation::value() {
  BoardState *state = pos.state();
  Score score = pos.getPSQTScore();

  Value im = (imbalance<WHITE>() - imbalance<BLACK>()) / 16;

  score += pe->score(WHITE) - pe->score(BLACK);

  score += _S(im, im);

  Value v = (score.mg + score.eg) / 2;
  if (std::abs(v) > LazyThreshold + pos.nonPawnMaterial() / 64)
    return pos.getSideToMove() == WHITE ? v : -v;

  initialise<WHITE>();
  initialise<BLACK>();

  score += pieces<WHITE, KNIGHT>() - pieces<BLACK, KNIGHT>() +
           pieces<WHITE, BISHOP>() - pieces<BLACK, BISHOP>() +
           pieces<WHITE, ROOK>() - pieces<BLACK, ROOK>() +
           pieces<WHITE, QUEEN>() - pieces<BLACK, QUEEN>();

  score += mobility[WHITE] - mobility[BLACK];

  score += king<WHITE>() - king<BLACK>() + threats<WHITE>() - threats<BLACK>() +
           passed<WHITE>() - passed<BLACK>() + space<WHITE>() - space<BLACK>();

  score += initiative(score);

  Value npm_w = state->nonPawnMaterial[WHITE] * 2;
  Value npm_b = state->nonPawnMaterial[BLACK] * 2;
  Value npm = clamp(npm_w + npm_b, EndgameLimit, MidgameLimit);

  int gamePhase = (npm - EndgameLimit) * 128 / (MidgameLimit - EndgameLimit);

  v = (score.mg * gamePhase + score.eg * (128 - gamePhase));

  v /= 128;

  v = (v * (100 - state->fiftyMove) / 100);

  return (pos.getSideToMove() == WHITE ? v : -v) + 28;
}

template <Colour Us> int Evaluation::imbalance() const {

  // Evaluate the material imbalance. We use PIECE_TYPE_NONE as a place holder
  // for the bishop pair "extended piece", which allows us to be more flexible
  // in defining bishop pair bonuses.
  const int pieceCount[COLOUR_N][PIECE_TYPE_N] = {
      {pos.getPieceCount(wB) > 1, pos.getPieceCount(wP), pos.getPieceCount(wN),
       pos.getPieceCount(wB), pos.getPieceCount(wR), pos.getPieceCount(wQ)},
      {pos.getPieceCount(bB) > 1, pos.getPieceCount(bP), pos.getPieceCount(bN),
       pos.getPieceCount(bB), pos.getPieceCount(bR), pos.getPieceCount(bQ)},
  };

  constexpr Colour Them = ~Us;

  int bonus = 0;

  Value v;

  // Second-degree polynomial material imbalance, by Tom Romstad
  for (int pt1 = NO_PIECE_TYPE; pt1 <= QUEEN; ++pt1) {
    if (!pieceCount[Us][pt1])
      continue;

    int v = 0;
    for (PieceType pt2 = NO_PIECE_TYPE; pt2 <= pt1; ++pt2) {
      v += QuadraticOurs[pt1][pt2] * pieceCount[Us][pt2] +
           QuadraticTheirs[pt1][pt2] * pieceCount[Them][pt2];
    }

    bonus += pieceCount[Us][pt1] * v;
  }

  return bonus;
}

Value eval(const Position &pos) { return Evaluation(pos).value(); }

// enum GamePhase { OPENING, MIDDLEGAME, ENDGAME };

// constexpr int getGamePhase(const Position &pos) {
//   // Define game phase
//   int gamePhase = 0;
//   // Loop through piece types
//   for (PieceType pt = KNIGHT; pt <= QUEEN; ++pt) {
//     int pieceCount = pos.getPieceCount(toPiece(WHITE, pt)) +
//                      pos.getPieceCount(toPiece(BLACK, pt));
//     gamePhase += pieceVal[pt].mg * pieceCount;
//   }
//   // Return gamephase
//   return gamePhase;
// }

// Value eval(const Position &pos) {

//   BoardState *state = pos.state();
//   PawnEntry *e = probe(pos);

//   Score material = state->pawns[WHITE] - state->pawns[BLACK] +
//                    state->nonPawnMaterial[WHITE] -
//                    state->nonPawnMaterial[BLACK];

//   Score positional = state->psqtVal;

//   Score pawnScore = e->score(WHITE) - e->score(BLACK);

//   Score kingSafety = e->kingSafety<WHITE>(pos) - e->kingSafety<BLACK>(pos);

//   Score total = material + positional;

//   int gamePhaseScore = getGamePhase(pos);

//   int gamePhase = MIDDLEGAME;
//   if (gamePhaseScore > 6192)
//     gamePhase = OPENING;
//   else if (gamePhaseScore < 518)
//     gamePhase = ENDGAME;

//   Value value =
//       ((total.mg * gamePhaseScore) + (total.eg * (6192 - gamePhaseScore))) /
//       6192;

//   return (pos.getSideToMove() == WHITE) ? value : -value;
// }

void addTrace(const Position &pos, const std::string &name, Score white,
              Score black) {
  std::cout << std::format("|{:^16}|{:^7}|{:^7}|{:^7}|{:^7}|{:^7}|{:^7}|\n",
                           name, white.mg, white.eg, black.mg, black.eg,
                           (white.mg - black.mg), (white.eg - black.eg));
}

// Trace scores
void Evaluation::trace() {

  BoardState *state = pos.state();

  Score material[COLOUR_N];

  initialise<WHITE>();
  initialise<BLACK>();

  Score piece_w = pieces<WHITE, KNIGHT>() + pieces<WHITE, BISHOP>() +
                  pieces<WHITE, ROOK>() + pieces<WHITE, QUEEN>();

  Score piece_b = pieces<BLACK, KNIGHT>() + pieces<BLACK, BISHOP>() +
                  pieces<BLACK, ROOK>() + pieces<BLACK, QUEEN>();

  std::cout << std::format("|{:^16}|{:^15}|{:^15}|{:^15}|\n", "Type", "White",
                           "Black", "Total");
  std::cout << std::format("|{:-^16}|{:-^15}|{:-^15}|{:-^15}|\n", "-", "-", "-",
                           "-");
  addTrace(pos, "Pawns", pe->score(WHITE), pe->score(BLACK));
  addTrace(pos, "Piece", piece_w, piece_b);
  addTrace(pos, "Mobility", mobility[WHITE], mobility[BLACK]);
  addTrace(pos, "King", king<WHITE>(), king<BLACK>());
  addTrace(pos, "Passed", passed<WHITE>(), passed<BLACK>());
  addTrace(pos, "Space", space<WHITE>(), space<BLACK>());
  addTrace(pos, "Threats", threats<WHITE>(), threats<BLACK>());
  addTrace(pos, "Imbalance", _S(imbalance<WHITE>(), imbalance<WHITE>()) / 16,
           _S(imbalance<BLACK>(), imbalance<BLACK>()) / 16);
}

void traceEval(const Position &pos) {
  Evaluation e(pos);
  e.trace();
}