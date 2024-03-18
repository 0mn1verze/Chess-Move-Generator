#ifndef PSQT_HPP
#define PSQT_HPP

#include "defs.hpp"
#include "eval.hpp"

// Piece Values (Stockfish 11)

Value PieceValue[PHASE_N][PIECE_N] = {{0, 128, 781, 825, 1276, 2538},
                                      {0, 213, 854, 915, 1380, 2682}};
namespace PSQT {
constexpr Score Bonus[][RANK_N][int(FILE_N) / 2] = {
    {},
    {},
    {// Knight
     {_S(-175, -96), _S(-92, -65), _S(-74, -49), _S(-73, -21)},
     {_S(-77, -67), _S(-41, -54), _S(-27, -18), _S(-15, 8)},
     {_S(-61, -40), _S(-17, -27), _S(6, -8), _S(12, 29)},
     {_S(-35, -35), _S(8, -2), _S(40, 13), _S(49, 28)},
     {_S(-34, -45), _S(13, -16), _S(44, 9), _S(51, 39)},
     {_S(-9, -51), _S(22, -44), _S(58, -16), _S(53, 17)},
     {_S(-67, -69), _S(-27, -50), _S(4, -51), _S(37, 12)},
     {_S(-201, -100), _S(-83, -88), _S(-56, -56), _S(-26, -17)}},
    {// Bishop
     {_S(-53, -57), _S(-5, -30), _S(-8, -37), _S(-23, -12)},
     {_S(-15, -37), _S(8, -13), _S(19, -17), _S(4, 1)},
     {_S(-7, -16), _S(21, -1), _S(-5, -2), _S(17, 10)},
     {_S(-5, -20), _S(11, -6), _S(25, 0), _S(39, 17)},
     {_S(-12, -17), _S(29, -1), _S(22, -14), _S(31, 15)},
     {_S(-16, -30), _S(6, 6), _S(1, 4), _S(11, 6)},
     {_S(-17, -31), _S(-14, -20), _S(5, -1), _S(0, 1)},
     {_S(-48, -46), _S(1, -42), _S(-14, -37), _S(-23, -24)}},
    {// Rook
     {_S(-31, -9), _S(-20, -13), _S(-14, -10), _S(-5, -9)},
     {_S(-21, -12), _S(-13, -9), _S(-8, -1), _S(6, -2)},
     {_S(-25, 6), _S(-11, -8), _S(-1, -2), _S(3, -6)},
     {_S(-13, -6), _S(-5, 1), _S(-4, -9), _S(-6, 7)},
     {_S(-27, -5), _S(-15, 8), _S(-4, 7), _S(3, -6)},
     {_S(-22, 6), _S(-2, 1), _S(6, -7), _S(12, 10)},
     {_S(-2, 4), _S(12, 5), _S(16, 20), _S(18, -5)},
     {_S(-17, 18), _S(-19, 0), _S(-1, 19), _S(9, 13)}},
    {// Queen
     {_S(3, -69), _S(-5, -57), _S(-5, -47), _S(4, -26)},
     {_S(-3, -55), _S(5, -31), _S(8, -22), _S(12, -4)},
     {_S(-3, -39), _S(6, -18), _S(13, -9), _S(7, 3)},
     {_S(4, -23), _S(5, -3), _S(9, 13), _S(8, 24)},
     {_S(0, -29), _S(14, -6), _S(12, 9), _S(5, 21)},
     {_S(-4, -38), _S(10, -18), _S(6, -12), _S(8, 1)},
     {_S(-5, -50), _S(6, -27), _S(10, -24), _S(8, -8)},
     {_S(-2, -75), _S(-2, -52), _S(1, -43), _S(-2, -36)}},
    {// King
     {_S(271, 1), _S(327, 45), _S(271, 85), _S(198, 76)},
     {_S(278, 53), _S(303, 100), _S(234, 133), _S(179, 135)},
     {_S(195, 88), _S(258, 130), _S(169, 169), _S(120, 175)},
     {_S(164, 103), _S(190, 156), _S(138, 172), _S(98, 172)},
     {_S(154, 96), _S(179, 166), _S(105, 199), _S(70, 199)},
     {_S(123, 92), _S(145, 172), _S(81, 184), _S(31, 191)},
     {_S(88, 47), _S(120, 121), _S(65, 116), _S(33, 131)},
     {_S(59, 11), _S(89, 59), _S(45, 73), _S(-1, 78)}}};

constexpr Score PBonus[RANK_N][FILE_N] = { // Pawn (asymmetric distribution)
    {},
    {_S(3, -10), _S(3, -6), _S(10, 10), _S(19, 0), _S(16, 14), _S(19, 7),
     _S(7, -5), _S(-5, -19)},
    {_S(-9, -10), _S(-15, -10), _S(11, -10), _S(15, 4), _S(32, 4), _S(22, 3),
     _S(5, -6), _S(-22, -4)},
    {_S(-8, 6), _S(-23, -2), _S(6, -8), _S(20, -4), _S(40, -13), _S(17, -12),
     _S(4, -10), _S(-12, -9)},
    {_S(13, 9), _S(0, 4), _S(-13, 3), _S(1, -12), _S(11, -12), _S(-2, -6),
     _S(-13, 13), _S(5, 8)},
    {_S(-5, 28), _S(-12, 20), _S(-7, 21), _S(22, 28), _S(-8, 30), _S(-5, 7),
     _S(-15, 6), _S(-18, 13)},
    {_S(-7, 0), _S(7, -11), _S(-3, 12), _S(-13, 21), _S(5, 25), _S(-16, 19),
     _S(10, 4), _S(-8, 7)}};

#undef S

Score psq[PIECE_N][SQ_N];

// init() initializes piece-square tables: the white halves of the tables are
// copied from Bonus[] adding the piece value, then the black halves of the
// tables are initialized by flipping and changing the sign of the white scores.
void init() {

  for (PieceType pt = PAWN; pt <= KING; ++pt) {
    PieceValue[MG][toPiece(BLACK, pt)] = PieceValue[MG][toPiece(WHITE, pt)];
    PieceValue[EG][toPiece(BLACK, pt)] = PieceValue[EG][toPiece(WHITE, pt)];

    Score score = _S(PieceValue[MG][toPiece(WHITE, pt)],
                     PieceValue[EG][toPiece(WHITE, pt)]);

    for (Square sq = A1; sq <= H8; ++sq) {
      File f = fileFromEdge(fileOf(sq));
      psq[toPiece(WHITE, pt)][sq] =
          score + (pt == PAWN ? PBonus[rankOf(sq)][fileOf(sq)]
                              : Bonus[toPiece(WHITE, pt)][rankOf(sq)][f]);
      psq[toPiece(BLACK, pt)][flipRank(sq)] = -psq[toPiece(WHITE, pt)][sq];
    }
  }
}

} // namespace PSQT

// clang-format on
#endif // PSQT_HPP