module Main where

import Lib
import Parse

main :: IO ()
main = do
  input <- getContents
  putStrLn input
  putStrLn $ show $ stringToProgram input
