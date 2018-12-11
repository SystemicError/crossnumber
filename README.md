# crossnumber

A Clojure library designed to create and solve "crossnumber" puzzles.

## Usage

Crossnumber puzzles are numerical analogues to crossword puzzles.  Where crosswords give row and column clues that relate to wordplay or trivia and have answers that are words, crossnumbers give row and column clues that are prime factorizations of answers that are numbers.

Here's an example of a crossnumber puzzle:

|          | (A) | (B^3)(C) |
|----------|-----|----------|
| (B^2)(C) | ? | ? |
| (B)(D)  | ? | ? |

and its solution:

|          | (13) | (2^3)(3) |
|----------|-----|----------|
| (2^2)(3) | 1 | 2 |
| (2)(17)  | 3 | 4 |

Notice that the letters used to represent each prime are consistent across all clues.  We also guarantee in a valid puzzle that no row or column starts with a leading zero.

## License

Copyright © 2018 Tevis Tsai.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
