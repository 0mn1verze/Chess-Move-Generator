#include <algorithm>
#include <cstring>
#include <iostream>
#include <sstream>

#include "bitboard.hpp"
#include "defs.hpp"
#include "hashtable.hpp"
#include "misc.hpp"
#include "movegen.hpp"
#include "position.hpp"
#include "psqt.hpp"

/******************************************\
|==========================================|
|             Zobrist Hashing              |
|==========================================|
\******************************************/

// https://www.chessprogramming.org/Zobrist_Hashing

namespace Zobrist {
Key pieceSquareKeys[PIECE_N][SQ_N]{};
Key enPassantKeys[FILE_N]{};
Key castlingKeys[CASTLING_N]{};
Key sideKey{};
} // namespace Zobrist

// Initialize zobrist keys
void initZobrist() {
  // Initialize piece square keys
  for (Piece pce : {wP, wN, wB, wR, wQ, wK, bP, bN, bB, bR, bR, bQ, bK})
    for (Square sq = A1; sq <= H8; ++sq)
      Zobrist::pieceSquareKeys[pce][sq] = getRandom<Key>();

  // Initialize enpassant keys
  for (File file = FILE_A; file <= FILE_H; ++file)
    Zobrist::enPassantKeys[file] = getRandom<Key>();

  // Initialize castling keys
  for (Castling c = NO_CASTLE; c <= ANY_SIDE; ++c)
    Zobrist::castlingKeys[c] = getRandom<Key>();

  // Initialize side key
  Zobrist::sideKey = getRandom<Key>();
}

/******************************************\
|==========================================|
|            Piece Manipulation            |
|==========================================|
\******************************************/

// Put piece on square
void Position::putPiece(Piece piece, Square sq) {
  // Put piece in piece list
  pieceList[sq] = piece;
  // Update piece bitboards
  piecesBB[ALL_PIECES] |= piecesBB[toPieceType(piece)] |= sq;
  // Update occupancy bitboards
  occupiedBB[toColour(piece)] |= sq;
  occupiedBB[BOTH] |= sq;
  // Update piece count list
  pieceCount[piece]++;
  pieceCount[toPiece(toColour(piece), ALL_PIECES)]++;
}

// Remove piece on square
void Position::popPiece(Square sq) {
  // Find piece on square
  Piece piece = pieceList[sq];
  // Update piece bitboards
  piecesBB[ALL_PIECES] ^= sq;
  piecesBB[toPieceType(piece)] ^= sq;
  // Update occupancy bitboards
  occupiedBB[toColour(piece)] ^= sq;
  occupiedBB[BOTH] ^= sq;
  // Update piece list at square
  pieceList[sq] = NO_PIECE;
  // Update piece count
  pieceCount[piece]--;
  pieceCount[toPiece(toColour(piece), ALL_PIECES)]--;
}

// Move piece between two squares without updating piece counts (Quicker)
void Position::movePiece(Square from, Square to) {
  // Find piece on square
  Piece piece = pieceList[from];
  // Update piece bitboards
  piecesBB[ALL_PIECES] ^= from | to;
  piecesBB[toPieceType(piece)] ^= from | to;
  // Update occupancy bitboards
  occupiedBB[toColour(piece)] ^= from | to;
  occupiedBB[BOTH] ^= from | to;
  // Update piece list
  pieceList[from] = NO_PIECE;
  pieceList[to] = piece;
}

/******************************************\
|==========================================|
|         Position Set Up Functions        |
|==========================================|
\******************************************/

// Returns ASCII representation of the board
void Position::print() const {
  // Row seperator
  const std::string seperator = "\n  +---+---+---+---+---+---+---+---+\n";

  // Result string
  std::cout << seperator;

  // Print bitboard
  for (Rank rank = RANK_8; rank >= RANK_1; --rank) {

    // Print rank number
    std::cout << rank + 1 << " ";

    for (File file = FILE_A; file <= FILE_H; ++file) {
      // Get square
      Square square = toSquare(file, rank);
      // Print bit on that square
      std::cout << "| " << piece2Str(getPiece(square)) << " ";
    }

    // Print bit on that square
    std::cout << "|" + seperator;
  }

  // Print files
  std::cout << "    a   b   c   d   e   f   g   h\n\n";

  // Print side to move
  std::cout << "Side to move: " << (sideToMove == WHITE ? "White" : "Black")
            << "\n";

  // Print castling rights
  std::cout << "Castling rights: ";
  // Print individual castling rights (Either KQkq or -)
  if (st->castling & WKCA)
    std::cout << "K";
  else
    std::cout << "-";
  if (st->castling & WQCA)
    std::cout << "Q";
  else
    std::cout << "-";
  if (st->castling & BKCA)
    std::cout << "k";
  else
    std::cout << "-";
  if (st->castling & BQCA)
    std::cout << "q";
  else
    std::cout << "-";

  std::cout << std::endl;
  // Print Enpassant square
  std::cout << "Enpassant Square: ";
  if (st->enPassant != NO_SQ)
    std::cout << sq2Str(st->enPassant) << std::endl;
  else
    std::cout << "None" << std::endl;

  // Print hash key
  std::cout << "Hash Key: " << std::hex << st->key << std::dec << std::endl;
}

// Set the position based on fen string
void Position::set(const std::string &fen, BoardState &state) {
  // Initialize square and piece index
  Square square = A8;
  size_t pieceIdx;
  // Create token to store tokens in the input stream
  unsigned char token;
  // If fen is empty, return empty position
  if (fen.empty())
    return;
  // Create input stream with fen strng to parse it
  std::istringstream is(fen);

  // Reset board state
  std::memset(this, 0, sizeof(Position));

  // Reset new state
  state = {};
  // Set board state
  this->st = &state;

  // Set no skip whitespace flag to detect white spaces
  is >> std::noskipws;

  // Parse board pieces (Loop while there are non-space tokens)
  while ((is >> token) && !isspace(token)) {

    // If token is '/' continue to next token
    if (token == '/')
      square += SS;

    // If token is a number, skip that many squares
    else if (isdigit(token))
      square += E * (token - '0');

    // If token is a piece, put it on the square
    else if ((pieceIdx = pieceToChar.find(token)) != std::string::npos) {
      // Put piece on the square
      putPiece(Piece(pieceIdx), square);
      // Move to next square
      ++square;
    }
  }

  // Parse side to move
  is >> token;
  sideToMove = (token == 'w') ? WHITE : BLACK;
  is >> token;

  // Parse castling rights
  while ((is >> token) && !isspace(token)) {
    switch (token) {
    case 'K':
      st->castling |= WKCA;
      break;
    case 'Q':
      st->castling |= WQCA;
      break;
    case 'k':
      st->castling |= BKCA;
      break;
    case 'q':
      st->castling |= BQCA;
      break;
    default:
      break;
    }
  }

  // Parse enpassant square
  unsigned char file, rank;

  if (((is >> file) && (file >= 'a' && file <= 'h')) &&
      ((is >> rank) && (rank == (sideToMove == WHITE ? '6' : '3'))))
    // Set enpassant square
    st->enPassant = toSquare(File(file - 'a'), Rank(rank - '1'));
  else
    // If enpassant square is not set, set it to NO_SQ
    st->enPassant = NO_SQ;

  // Parse halfmove clock
  is >> std::skipws >> st->fiftyMove;

  // initialise hash key
  st->key = initKey();

  // initialise score
  initScore();

  Bitboard pawns = getPiecesBB(~sideToMove, PAWN);
  Bitboard knights = getPiecesBB(~sideToMove, KNIGHT);

  Bitboard attacks;

  while (pawns) {
    Square sq = popLSB(pawns);
    if (~sideToMove == WHITE) {
      attacks = pawnAttacksBB<WHITE>(sq);
    } else {
      attacks = pawnAttacksBB<BLACK>(sq);
    }

    if (attacks & getPiecesBB(sideToMove, KING)) {
      st->checkMask = squareBB(sq);
    }
  }

  while (knights) {
    Square sq = popLSB(knights);
    attacks = attacksBB<KNIGHT>(sq, EMPTYBB);
    if (attacks & getPiecesBB(sideToMove, KING)) {
      st->checkMask = squareBB(sq);
    }
  }

  // Refresh masks
  refreshMasks(*this);
}

void Position::setState(BoardState &state) { st = &state; }

Key Position::initKey() const {
  // Initialize hash key
  Key key = 0ULL;
  // Loop through all squares
  for (Square square = A1; square <= H8; ++square) {
    // Get piece on the square
    Piece piece = getPiece(square);
    // If there is a piece on the square, update hash key
    if (piece != NO_PIECE)
      key ^= Zobrist::pieceSquareKeys[piece][square];
  }
  // Update hash key with side to move
  if (sideToMove == BLACK)
    key ^= Zobrist::sideKey;
  // Update hash key with castling rights
  key ^= Zobrist::castlingKeys[st->castling];
  // Update hash key with enpassant square
  if (st->enPassant != NO_SQ)
    key ^= Zobrist::enPassantKeys[fileOf(st->enPassant)];

  return key;
}

void Position::initScore() const {
  st->psqtVal = _S(0, 0);
  st->pawns[WHITE] = pieceVal[PAWN] * getPieceCount(wP);
  st->pawns[BLACK] = pieceVal[PAWN] * getPieceCount(bP);

  st->nonPawnMaterial[WHITE] = _S(0, 0);
  st->nonPawnMaterial[BLACK] = _S(0, 0);

  for (PieceType pt = KNIGHT; pt <= KING; ++pt) {
    st->nonPawnMaterial[WHITE] +=
        pieceVal[pt] * getPieceCount(toPiece(WHITE, pt));
    st->nonPawnMaterial[BLACK] +=
        pieceVal[pt] * getPieceCount(toPiece(BLACK, pt));
  }

  Square sq = A1;
  for (Colour side : {WHITE, BLACK}) {
    for (PieceType pt = PAWN; pt <= KING; ++pt) {
      Bitboard pieces = getPiecesBB(side, pt);

      while (pieces) {
        Square sq = popLSB(pieces);
        st->psqtVal +=
            relativeScore(side, psqTable[pt][relativeRank(side, sq)]);
      }
    }
  }
}

/******************************************\
|==========================================|
|             Board functions              |
|==========================================|
\******************************************/

// Bitboard Position::sqAttackedByBB(Square sq, Bitboard occupied) const {
//   return (pawnAttacksBB<BLACK>(sq) & getPiecesBB(WHITE, PAWN)) |
//          (pawnAttacksBB<WHITE>(sq) & getPiecesBB(BLACK, PAWN)) |
//          (attacksBB<KNIGHT>(sq, occupied) & getPiecesBB(KNIGHT)) |
//          (attacksBB<BISHOP>(sq, occupied) & getPiecesBB(BISHOP, QUEEN)) |
//          (attacksBB<ROOK>(sq, occupied) & getPiecesBB(ROOK, QUEEN)) |
//          (attacksBB<KING>(sq, occupied) & getPiecesBB(KING));
// }

// Private functions
Bitboard Position::attackedByBB(Colour enemy) const {
  Square attacker;
  Bitboard attacks = EMPTYBB;

  // Knight attacks
  Bitboard knights = getPiecesBB(enemy, KNIGHT);
  while (knights) {
    attacker = popLSB(knights);
    attacks |= attacksBB<KNIGHT>(attacker, EMPTYBB);
  }

  // Pawn attacks
  Bitboard pawns = getPiecesBB(enemy, PAWN);
  attacks |= (enemy == WHITE) ? pawnAttacksBB<WHITE>(pawns)
                              : pawnAttacksBB<BLACK>(pawns);

  // King attacks
  Bitboard kings = getPiecesBB(enemy, KING);
  attacks |= attacksBB<KING>(popLSB(kings), EMPTYBB);

  // Bishop and Queen attacks
  Bitboard bishops = getPiecesBB(enemy, BISHOP, QUEEN);
  while (bishops) {
    attacker = popLSB(bishops);
    attacks |= attacksBB<BISHOP>(attacker, getOccupiedBB());
  }

  // Rook and Queen attacks
  Bitboard rooks = getPiecesBB(enemy, ROOK, QUEEN);
  while (rooks) {
    attacker = popLSB(rooks);
    attacks |= attacksBB<ROOK>(attacker, getOccupiedBB());
  }

  return attacks;
}

bool Position::isDraw(int ply) const {
  if (st->fiftyMove > 99)
    return true;
  return st->repetition and st->repetition < ply;
}

bool Position::isCapture(Move move) const { return getPiece(move.to()); }

/******************************************\
|==========================================|
|               Make/Unmake                |
|==========================================|
\******************************************/

void Position::makeMove(Move move, BoardState &state) {
  // Reset new state
  state = {};
  // Copy current board state to new state partially
  std::memcpy(&state, st, offsetof(BoardState, key));

  // Get Hash Key (And change sides)
  Key hashKey = st->key ^ Zobrist::sideKey;

  state.previous = st;
  st = &state;

  ++st->fiftyMove;
  ++st->plies;
  ++pliesFromStart;

  // Get move variables
  const Colour side = sideToMove;
  const Colour enemy = ~side;
  const Square from = move.from();
  const Square to = move.to();
  const Piece piece = getPiece(from);
  const Piece cap = move.isEnPassant() ? toPiece(enemy, PAWN) : getPiece(to);

  // Handle castling
  if (move.isCastle()) {
    // Move rook
    Square rookFrom, rookTo;
    Piece rook = toPiece(side, ROOK);
    castleRook<true>(from, to, rookFrom, rookTo);
    // Update hash key
    hashKey ^= Zobrist::pieceSquareKeys[rook][rookFrom] ^
               Zobrist::pieceSquareKeys[rook][rookTo];

    st->psqtVal +=
        relativeScore(side, psqTable[ROOK][relativeRank(side, rookTo)] -
                                psqTable[ROOK][relativeRank(side, rookFrom)]);
  }

  // Handle captures
  if (cap != NO_PIECE) {
    Square capSq = to;
    // Handle enpassant
    if (move.isEnPassant()) {
      // Change captured square to the enpassant square
      capSq += (sideToMove == WHITE ? S : N);
    }
    // Update board by removing piece on the destination
    popPiece(capSq);
    occupiedBB[enemy] &= ~squareBB(capSq);
    // Update hash key
    hashKey ^= Zobrist::pieceSquareKeys[cap][capSq];
    // Update psqt score
    st->psqtVal -= relativeScore(
        enemy, psqTable[toPieceType(cap)][relativeRank(enemy, capSq)]);

    if (toPieceType(cap) == PAWN) {
      st->pawns[enemy] -= pieceVal[PAWN];
    } else {
      st->nonPawnMaterial[enemy] -= pieceVal[toPieceType(cap)];
    }

    // Update board state to undo move
    st->captured = cap;
  } else
    st->captured = NO_PIECE;

  // Update enpassant square
  if (st->enPassant != NO_SQ) {
    // Update hash key
    hashKey ^= Zobrist::enPassantKeys[fileOf(st->enPassant)];
    // Remove enpassant square
    st->enPassant = NO_SQ;
  }

  // Enpassant Square update
  if (toPieceType(piece) == PAWN && std::abs(rankOf(to) - rankOf(from)) == 2) {
    st->enPassant = from + ((sideToMove == WHITE) ? N : S);
    // Update hash key
    hashKey ^= Zobrist::enPassantKeys[fileOf(st->enPassant)];
  }

  // Update board by moving piece to the destination
  movePiece(from, to);
  // Update hash key
  hashKey ^= Zobrist::pieceSquareKeys[piece][from] ^
             Zobrist::pieceSquareKeys[piece][to];

  st->psqtVal += relativeScore(
      side, psqTable[toPieceType(piece)][relativeRank(side, to)] -
                psqTable[toPieceType(piece)][relativeRank(side, from)]);

  if (toPieceType(piece) == PAWN) {
    // Handle promotions
    if (move.isPromotion()) {
      Piece promotedTo = toPiece(side, move.promoted());
      // Swap the piece on the promotion square
      popPiece(to);
      putPiece(promotedTo, to);

      // Update check mask for promotions
      if (toPieceType(promotedTo) == KNIGHT &&
          attacksBB<KNIGHT>(to, EMPTYBB) & getPiecesBB(enemy, KING))
        st->checkMask = squareBB(to);

      // Update hash key
      hashKey ^= Zobrist::pieceSquareKeys[piece][to] ^
                 Zobrist::pieceSquareKeys[promotedTo][to];

      st->pawns[side] -= pieceVal[PAWN];
      st->nonPawnMaterial[side] += pieceVal[move.promoted()];

      st->psqtVal += relativeScore(
          side, psqTable[toPieceType(promotedTo)][relativeRank(side, to)] -
                    psqTable[PAWN][relativeRank(side, to)]);
    }
    // Update fifty move rule
    st->fiftyMove = 0;

    // Handle pawn checks to update check mask
    Bitboard pawnAttack =
        (side == WHITE) ? pawnAttacksBB<WHITE>(to) : pawnAttacksBB<BLACK>(to);
    if (pawnAttack & getPiecesBB(enemy, KING))
      st->checkMask = squareBB(to);
  }

  // Update checkmask for piece moves
  if (toPieceType(piece) == KNIGHT &&
      attacksBB<KNIGHT>(to, EMPTYBB) & getPiecesBB(enemy, KING))
    st->checkMask = squareBB(to);

  // Update hash key (Remove previous castling rights)
  hashKey ^= Zobrist::castlingKeys[st->castling];
  // Update castling flag (By Code Monkey King)
  st->castling &= castlingRights[from] & castlingRights[to];
  // Update hash key (Add new castling rights)
  hashKey ^= Zobrist::castlingKeys[st->castling];

  // Update side to move
  sideToMove = ~sideToMove;

  // Update state hash key
  st->key = hashKey;

  // Update move
  st->move = move;

  // Update repetition
  st->repetition = 0;

  int end = std::min(st->plies, st->fiftyMove);
  if (end >= 4) {
    // Go back 4 ply's (2 moves) to check for repetitions
    BoardState *prev = st->previous->previous;
    for (int i = 4; i <= end; i += 2) {
      prev = prev->previous->previous;
      // Check for repetition, if the previous position is also repeated then
      // set the repetition state to negative
      if (prev->key == st->key) {
        st->repetition = (prev->repetition ? -i : i);
        break;
      }
    }
  }

  refreshMasks(*this);

  TTPrefetch(st->key);
}

void Position::unmakeMove() {
  Move move = st->move;
  // Restore side to move
  sideToMove = ~sideToMove;

  // Get move variables
  const Colour side = sideToMove;
  const Colour enemy = ~side;
  Square from = move.from();
  Square to = move.to();
  Piece piece = getPiece(to);

  // Restore promotions
  if (move.isPromotion()) {
    popPiece(to);
    putPiece(toPiece(side, PAWN), to);
  }

  // Restore castling
  if (move.isCastle()) {
    Square rookFrom, rookTo;
    castleRook<false>(from, to, rookFrom, rookTo);
  }

  // Restore move
  movePiece(to, from);

  // Restore captures
  if (st->captured != NO_PIECE) {
    Square capSq = to;
    // Restore enpassant
    if (move.isEnPassant()) {
      capSq += (sideToMove ? N : S);
    }

    // Restore captured piece
    putPiece(st->captured, capSq);
  }

  st = st->previous;
  --pliesFromStart;
}

void Position::makeNullMove(BoardState &state) {
  state = {};
  // Copy current board state to new state partially
  std::memcpy(&state, st, sizeof(BoardState));

  state.previous = st;
  st = &state;

  if (st->enPassant != NO_SQ) {
    st->key ^= Zobrist::enPassantKeys[fileOf(st->enPassant)];
    st->enPassant = NO_SQ;
  }

  st->key ^= Zobrist::sideKey;
  ++st->fiftyMove;
  st->plies = 0;

  // Update side to move
  sideToMove = ~sideToMove;

  st->move = Move::null();

  // Update repetition
  st->repetition = 0;

  refreshMasks(*this);

  TTPrefetch(st->key);
}

void Position::unmakeNullMove() {

  // Restore board state
  st = st->previous;

  // Restore side to move
  sideToMove = ~sideToMove;
}

// bool Position::staticExchangeEvaluation(Move m, Value threshold) const {

//   if (!m.isNormal())
//     return threshold < 0;

//   Square from = m.from(), to = m.to();

//   int swap = pieceVal[getPiece(to)].mg - threshold;

//   // If the captured piece is not valuable enough, then return false
//   if (swap < 0)
//     return false;

//   swap = pieceVal[getPiece(from)].mg - swap;

//   // If the attacker is within the threshold of the capturing victim, return
//   // true
//   if (swap <= 0)
//     return true;

//   Bitboard occupied = getOccupiedBB() ^ from ^ to;
//   Colour stm = sideToMove;
//   Bitboard attackers = sqAttackedByBB(to, occupied);
//   Bitboard stmAttackers, bb;
//   bool res = 1;
//   bool king;

//   while (true) {
//     stm = ~stm;
//     attackers &= occupied;

//     // If the side to move has no more attackers then give up
//     if (!(stmAttackers = attackers & getOccupiedBB(stm)))
//       break;

//     // Don't allowed pinned pieces to attack as long as there are pinners on
//     // their original square
//     if (st->pinners[~stm] & occupied) {
//       stmAttackers &= st->pinned[stm];

//       if (!stmAttackers)
//         break;
//     }

//     res ^= 1;

//     // Find least valuable attacker
//     for (PieceType pt = PAWN; pt <= QUEEN; ++pt) {
//       if ((bb = stmAttackers & getPiecesBB(pt))) {
//         if ((swap = pieceVal[pt].mg - swap) < res) {
//           return res;
//         }
//         // Remove attacker from square
//         occupied ^= getLSB(bb);

//         attackers |=
//             attacksBB<BISHOP>(to, occupied) & getPiecesBB(BISHOP, QUEEN) |
//             attacksBB<ROOK>(to, occupied) & getPiecesBB(ROOK, QUEEN);
//         king = false;
//         break;
//       }
//     }

//     // If we capture with the king but the opponent still has attackers,
//     reverse
//     // the result
//     if (king)
//       return (attackers & ~getOccupiedBB(stm)) ? res ^ 1 : res;
//   }

//   return res;
// }