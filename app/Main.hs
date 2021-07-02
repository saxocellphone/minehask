module Main where

import Game
import System.Environment 

main :: IO ()
main = do
  args <- getArgs
  let [w, h, numMines] = args
  play (read w) (read h) (read numMines)
