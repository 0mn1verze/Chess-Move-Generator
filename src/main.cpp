#include <cstring>
#include <iostream>

#include "bitboard.hpp"
#include "defs.hpp"
#include "movegen.hpp"
#include "perft.hpp"
#include "position.hpp"

int main() {

  initBitboards();

  Position pos;
  BoardState st{};
  pos.set(startPos.data(), st);

  perftTest(pos, 6);

  return 0;
}