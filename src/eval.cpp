#include <iostream>

#include "defs.hpp"
#include "eval.hpp"
#include "misc.hpp"
#include "position.hpp"

enum GamePhase {
  OPENING = 0,
  MIDDLEGAME = 1,
  ENDGAME = 2,
};

constexpr int getGamePhase(const Position &pos) {
  // Define game phase
  int gamePhase = 0;
  // Loop through piece types
  for (PieceType pt = KNIGHT; pt <= QUEEN; ++pt) {
    int pieceCount = pos.getPieceCount(toPiece(WHITE, pt)) +
                     pos.getPieceCount(toPiece(BLACK, pt));
    gamePhase += pieceVal[pt].mg * pieceCount;
  }
  // Return gamephase
  return gamePhase;
}

Value eval(Position &pos) {

  BoardState *state = pos.state();

  Score pawnScore = state->pawns[WHITE] - state->pawns[BLACK];
  Score nonPawnScore =
      state->nonPawnMaterial[WHITE] - state->nonPawnMaterial[BLACK];
  Score psqtScore = state->psqtVal;

  Score total = pawnScore + nonPawnScore + psqtScore;

  int gamePhaseScore = getGamePhase(pos);

  int gamePhase = MIDDLEGAME;
  if (gamePhaseScore > 6192)
    gamePhase = OPENING;
  else if (gamePhaseScore < 518)
    gamePhase = ENDGAME;

  Value value =
      ((total.mg * gamePhaseScore) + (total.eg * (6192 - gamePhaseScore))) /
      6192;

  return (pos.getSideToMove() == WHITE) ? value : -value;
}