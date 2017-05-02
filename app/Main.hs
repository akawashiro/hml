module Main where

import Lib
import Parse

main :: IO ()
main = do
  putStrLn $ show $ stringToProgram "1 + 2;;"
