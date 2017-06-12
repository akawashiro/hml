module Main where

import Parse
import Eval
import Control.Monad
import Alpha

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "input = \n" ++ input ++ "\n"
  putStrLn $ "after parsing = \n" ++ (show $ stringToProgram input) ++ "\n"
  putStrLn $ "after alpha conversion = \n" 
             ++ (show $ programToAlphaProgram `liftM` (stringToProgram input)) ++ "\n"
  print $ programToExVal `liftM` programToAlphaProgram `liftM` stringToProgram input
