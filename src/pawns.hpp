#ifndef PAWNS_HPP
#define PAWNS_HPP

#include "bitboard.hpp"
#include "defs.hpp"
#include "position.hpp"
#include <vector>

struct PawnEntry {
  Score score(Colour c) const { return scores[c]; }
  Bitboard pawnAttacks(Colour c) const { return attacks[c]; }
  Bitboard pawnAttacksSpan(Colour c) const { return attackSpan[c]; }
  Bitboard passedPawns(Colour c) const { return passedPawnsBB[c]; }
  int passedPawnsCount() const {
    return countBits(passedPawnsBB[WHITE] | passedPawnsBB[BLACK]);
  }

  template <Colour us> Score kingSafety(const Position &pos) {
    return kingSq[us] == pos.kingSquare(us) and
                   castlingRights[us] == pos.castling(us)
               ? kingSafetyScore[us]
               : (kingSafetyScore[us] = doKingSafety<us>(pos));
  }

  template <Colour us> Score doKingSafety(const Position &pos);

  template <Colour us> Score evaluateShelter(const Position &pos, Square ksq);

  Key key;
  Score scores[COLOUR_N]{};
  Bitboard passedPawnsBB[COLOUR_N]{};
  Bitboard attacks[COLOUR_N]{};
  Bitboard attackSpan[COLOUR_N]{};
  Square kingSq[COLOUR_N]{};
  Score kingSafetyScore[COLOUR_N]{};
  int castlingRights[COLOUR_N]{};
};

struct PawnTable {
  PawnEntry *operator[](Key key) {
    return &table[(std::uint32_t)key & ((1 << 17) - 1)];
  }

private:
  std::vector<PawnEntry> table = std::vector<PawnEntry>(1 << 17);
};

PawnEntry *probe(const Position &pos);

extern PawnTable pawnTable;
#endif // PAWNS_HPP