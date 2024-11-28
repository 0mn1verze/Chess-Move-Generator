#ifndef HASH_HPP
#define HASH_HPP

#include "defs.hpp"
#include "move.hpp"

namespace Maestro {

/******************************************\
|==========================================|
|             Zobrist Hashing              |
|==========================================|
\******************************************/

// https://www.chessprogramming.org/Zobrist_Hashing

namespace Zobrist {
extern Key pieceSquareKeys[PIECE_N][SQ_N];
extern Key enPassantKeys[FILE_N];
extern Key castlingKeys[CASTLING_N];
extern Key sideKey;

void init();

} // namespace Zobrist

// Init zobrist hashing

} // namespace Maestro

#endif // HASH_HPP
