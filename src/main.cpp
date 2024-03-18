#include <cstring>
#include <iostream>

#include "bitboard.hpp"
#include "defs.hpp"
#include "eval.hpp"
#include "movegen.hpp"
#include "movepicker.hpp"
#include "perft.hpp"
#include "position.hpp"
#include "uci.hpp"

int main() {
  UCI uci;
  uci.loop();

  return 0;
}