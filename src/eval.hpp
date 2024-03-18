#ifndef EVAL_HPP
#define EVAL_HPP

#include "defs.hpp"
#include "position.hpp"

enum Phase { MG, EG, PHASE_N };

extern Value PieceValue[PHASE_N][PIECE_N];

namespace PSQT {
extern Score psq[PIECE_N][SQ_N];
void init();
} // namespace PSQT

Value eval(const Position &pos);

void traceEval(const Position &pos);

#endif // EVAL_HPP