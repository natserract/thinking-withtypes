module Main where

-- Blocks of chapter modules
import Modules.Chapter01 (ticTacToeWinner)
import Other.Function 

-- Blocks of other modules

main :: IO ()
main = do
  print $ sign 10
  print $ modernise "the morphology of prex"
  print $ letFun
  print $ ticTacToeWinner
  
