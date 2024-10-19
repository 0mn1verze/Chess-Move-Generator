# Maestro Chess Engine

This is a chess engine project I used to learn c++ programming and search algorithms like minimax, alpha beta pruning etc.

> [!WARNING] 
> Some parts of this engine is copied from other open source engines (Ethreal and Stockfish)

## Features in this engine:
- Fast move generation (250-350Mnps on i7-10750H 2.4Ghz with turbo boost enabled)
- Iterative deepening (With aspiration windows)
- Time control
- Negamax search
- Null move pruning
- Late move reduction
- Static evaluation pruning
- Futility pruning
- Late move pruning
- Move ordering (MVV_LVA, History Heuristics, Killer Heuristics)
- Principal variation search
- Transposition Table (With buckets)
- Input listening thread
- Time management based on number of best move changes

## To do:
- Fix some bugs related to mate search
- Fix some bugs in the evaluation as the engine misses some important tactics sometimes
