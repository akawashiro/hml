module Main where

import Parse
import Eval
import Control.Monad

main :: IO ()
main = do
  input <- getContents
  -- putStrLn input
  print $ stringToProgram input
  print $ programToExVal `liftM` stringToProgram input
