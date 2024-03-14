#ifndef BITBOARD_HPP
#define BITBOARD_HPP

// C++ Standard Library
#include <immintrin.h>

#include "defs.hpp"

/******************************************\
|==========================================|
|           Bitboard functions             |
|==========================================|
\******************************************/

// Bitboard initialization function
void initBitboards();

// Bitboard print function
void printBitboard(Bitboard bb);

// Count the number of set bits in a bitboard
inline int countBits(Bitboard bb) { return _mm_popcnt_u64(bb); }

// Get the least significant bit from a bitboard
inline Square getLSB(Bitboard bb) { return (Square)_tzcnt_u64(bb); }

// Pop the least significant bit from a bitboard
inline Square popLSB(Bitboard &bb) {
  Square lsb = getLSB(bb);
  bb = _blsr_u64(bb);
  return lsb;
}

// Parallel bit extraction
inline Square pext(Bitboard bb, Bitboard mask) {
  return (Square)_pext_u64(bb, mask);
}

// More than one bit set
inline bool moreThanOne(Bitboard bb) { return (bool)_blsr_u64(bb); }

/******************************************\
|==========================================|
|           Bitboard Constants             |
|==========================================|
\******************************************/

// Bitboard constants
constexpr Bitboard FILEABB = 0x0101010101010101ULL;
constexpr Bitboard RANK1BB = 0xFFULL;
constexpr Bitboard EMPTYBB = 0ULL;
constexpr Bitboard FULLBB = ~EMPTYBB;

/******************************************\
|==========================================|
|              Lookup Tables               |
|==========================================|
\******************************************/

struct Magic {
  // Pointer to attacks table block
  Bitboard *attacks;
  // Attack mask for slider piece on a particular square
  Bitboard mask;
  // Calculate index in attacks table
  unsigned int index(Bitboard occupied) { return pext(occupied, mask); }
  Bitboard operator[](Bitboard occupied) { return attacks[index(occupied)]; }
};

// Pseudo attacks for all pieces except pawns
extern Bitboard pseudoAttacks[PIECE_TYPE_N][SQ_N];

// Bishop and rook magics
extern Magic bishopAttacks[SQ_N];
extern Magic rookAttacks[SQ_N];

// Bishop and rook attack tables
extern Bitboard bishopTable[0x1480];
extern Bitboard rookTable[0x19000];

// Line the two squares lie on [from][to]
extern Bitboard lineBB[SQ_N][SQ_N];
extern Bitboard betweenBB[SQ_N][SQ_N];

// Pins and checks [king][attacker]
extern Bitboard pinBB[SQ_N][SQ_N];
extern Bitboard checkBB[SQ_N][SQ_N];

// Castling rights lookup table
extern Castling castlingRights[SQ_N];

/******************************************\
|==========================================|
|           Bitboard Operators             |
|==========================================|
\******************************************/

// Get square bitboard
constexpr Bitboard squareBB(Square sq) { return 1ULL << sq; }
// Get rank bitboard
constexpr Bitboard rankBB(Rank r) { return RANK1BB << (r << 3); }
// Get rank bitboard (square)
constexpr Bitboard rankBB(Square sq) { return rankBB(rankOf(sq)); }
// Get file bitboard
constexpr Bitboard fileBB(File f) { return FILEABB << f; }
// Get rank bitboard (square)
constexpr Bitboard fileBB(Square sq) { return fileBB(fileOf(sq)); }
// Bitwise AND operation on bitboards (Get Bit)
constexpr Bitboard operator&(Bitboard bb, Square sq) {
  return bb & squareBB(sq);
}
// Bitwise OR opeartor on bitboards (Set bit)
constexpr Bitboard operator|(Bitboard bb, Square sq) {
  return bb | squareBB(sq);
}
// Bitwise XOR operator on bitboards (Toggle bit)
constexpr Bitboard operator^(Bitboard bb, Square sq) {
  return bb ^ squareBB(sq);
}

// Bitwise OR opeartor on bitboards (Combine squares)
constexpr Bitboard operator|(Square sq1, Square sq2) {
  return squareBB(sq1) | squareBB(sq2);
}
// Bitwise assignment OR operator on bitboards (Set Bit)
constexpr Bitboard &operator|=(Bitboard &bb, Square sq) {
  return bb |= squareBB(sq);
}
// Bitwise assignment XOR operator on bitboards (Toggle Bit)
constexpr Bitboard &operator^=(Bitboard &bb, Square sq) {
  return bb ^= squareBB(sq);
}

/******************************************\
|==========================================|
|            Bitboard Helpers              |
|==========================================|
\******************************************/

// Shift bitboards
template <Direction d> constexpr inline Bitboard shift(Bitboard bb) {
  // Returns the bitboard shift with checking for edges
  switch (d) {
  case N:
    return bb << 8;
  case S:
    return bb >> 8;
  case E:
    return (bb & ~fileBB(FILE_H)) << 1;
  case W:
    return (bb & ~fileBB(FILE_A)) >> 1;
  case NE:
    return (bb & ~fileBB(FILE_H)) << 9;
  case NW:
    return (bb & ~fileBB(FILE_A)) << 7;
  case SE:
    return (bb & ~fileBB(FILE_H)) >> 7;
  case SW:
    return (bb & ~fileBB(FILE_A)) >> 9;
  case NN:
    return bb << 16;
  case SS:
    return bb >> 16;
  case NNE:
    return (bb & ~fileBB(FILE_H)) << 17;
  case NNW:
    return (bb & ~fileBB(FILE_A)) << 15;
  case NEE:
    return (bb & ~(fileBB(FILE_H) | fileBB(FILE_G))) << 10;
  case NWW:
    return (bb & ~(fileBB(FILE_A) | fileBB(FILE_B))) << 6;
  case SEE:
    return (bb & ~(fileBB(FILE_H) | fileBB(FILE_G))) >> 6;
  case SWW:
    return (bb & ~(fileBB(FILE_A) | fileBB(FILE_B))) >> 10;
  case SSE:
    return (bb & ~fileBB(FILE_H)) >> 15;
  case SSW:
    return (bb & ~fileBB(FILE_A)) >> 17;
  default:
    return 0ULL;
  }
}

/******************************************\
|==========================================|
|              Attack Lookup               |
|==========================================|
\******************************************/

template <PieceType pt> Bitboard attacksBB(Square sq, Bitboard occupied) {
  switch (pt) {
  case KNIGHT:
    return pseudoAttacks[KNIGHT][sq];
  case KING:
    return pseudoAttacks[KING][sq];
  case BISHOP:
    return bishopAttacks[sq][occupied];
  case ROOK:
    return rookAttacks[sq][occupied];
  case QUEEN:
    return bishopAttacks[sq][occupied] | rookAttacks[sq][occupied];
  default:
    return Bitboard(0);
  }
}

template <Colour c> Bitboard pawnAttacksBB(Bitboard bb) {
  return (c == WHITE) ? shift<NW>(bb) | shift<NE>(bb)
                      : shift<SW>(bb) | shift<SE>(bb);
}

template <Colour c> Bitboard pawnAttacksBB(Square sq) {
  return pawnAttacksBB<c>(squareBB(sq));
}

#endif // BITBOARD_HPP