# Minehask - CLI Minesweeper written in haskell

## To play
```
stack build
stack exec -- minehask-exe <width> <height> <numMines>
```
## Demo
```
Make a move: (Format Int Int)
5 3
------------------
. . . . . 1
. . . . . 1
. 2 1 2 1 1 1 1 1
1 1         1 . .
      1 1 1 1 1 .
    1 2 . 1   1 .
    1 . . 1   1 .
  1 2 . . 2 2 2 .
  1 . . . . . . .
------------------
Make a move: (Format Int Int)
1 1
------------------
  1 . . . 1
1 2 . . . 1
. 2 1 2 1 1 1 1 1
1 1         1 . .
      1 1 1 1 1 .
    1 2 . 1   1 .
    1 . . 1   1 .
  1 2 . . 2 2 2 .
  1 . . . . . . .
------------------
Make a move: (Format Int Int)
2 3
"You Lost :("
------------------
  1 1 2 1 1
1 2 x 2 x 1
x 2 1 2 1 1 1 1 1
1 1         1 x 1
      1 1 1 1 1 1
    1 2 x 1   1 1
    1 x 2 1   1 x
  1 2 2 2 2 2 2 1
  1 x 1 1 x x 1
------------------
```
## TODOs
- Safe parse input
- row/col numbers
- GUI
