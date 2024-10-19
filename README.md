# Maestro Chess Engine

This is a chess move generator project I used to learn c++ programming.

## Features in this engine:
- Fast move generation (200-400Mnps on i7-10750H 2.4Ghz with turbo boost enabled)
- Full legal move generation using pin masks and check masks

## Example result
```
Perft Test: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1  Depth: 6 Nodes: 119060324 Passed in 446 ms with 266 Mnps
Perft Test: r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1  Depth: 5 Nodes: 193690690 Passed in 481 ms with 402 Mnps
Perft Test: 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - Depth: 7 Nodes: 178633661 Passed in 881 ms with 202 Mnps
Perft Test: r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1 Depth: 6 Nodes: 706045033 Passed in 1804 ms with 391 Mnps
Perft Test: 1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1 Depth: 5 Nodes: 1063513 Passed in 6 ms with 177 Mnps
Perft Test: 3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1 Depth: 6 Nodes: 1134888 Passed in 10 ms with 113 Mnps
Perft Test: 8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1 Depth: 6 Nodes: 1015133 Passed in 8 ms with 126 Mnps
Perft Test: 8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1 Depth: 6 Nodes: 1440467 Passed in 11 ms with 130 Mnps
Perft Test: 5k2/8/8/8/8/8/8/4K2R w K - 0 1 Depth: 6 Nodes: 661072 Passed in 6 ms with 110 Mnps
Perft Test: 3k4/8/8/8/8/8/8/R3K3 w Q - 0 1 Depth: 6 Nodes: 803711 Passed in 7 ms with 114 Mnps
Perft Test: r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1 Depth: 4 Nodes: 1274206 Passed in 4 ms with 318 Mnps
Perft Test: r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1 Depth: 4 Nodes: 1720476 Passed in 5 ms with 344 Mnps
Perft Test: 2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1 Depth: 6 Nodes: 3821001 Passed in 18 ms with 212 Mnps
Perft Test: 8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1 Depth: 5 Nodes: 1004658 Passed in 2 ms with 502 Mnps
Perft Test: 4k3/1P6/8/8/8/8/K7/8 w - - 0 1 Depth: 6 Nodes: 217342 Passed in 2 ms with 108 Mnps
Perft Test: 8/P1k5/K7/8/8/8/8/8 w - - 0 1 Depth: 6 Nodes: 92683 Passed in 1 ms with 92 Mnps
Perft Test: K1k5/8/P7/8/8/8/8/8 w - - 0 1 Depth: 6 Nodes: 2217 Passed in 1 ms with 2 Mnps
Perft Test: 8/k1P5/8/1K6/8/8/8/8 w - - 0 1 Depth: 7 Nodes: 567584 Passed in 3 ms with 189 Mnps
Perft Test: 8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1 Depth: 4 Nodes: 23527 Passed in 1 ms with 23 Mnps
```
