#include <cstring>
#include <iostream>

#include "bitboard.hpp"
#include "defs.hpp"
#include "hash.hpp"
#include "movegen.hpp"
#include "perft.hpp"
#include "position.hpp"

using namespace Maestro;

int main() {

  initBitboards();
  Zobrist::init();

  Position pos;
  BoardState st{};
  const std::string fen(startPos);
  pos.set(fen, st);

  perftTest(pos, 8);

  // perftBench("bench.csv");

  return 0;
}