module Main where

import Parse
import Eval
import Control.Monad

printResult (Right xs) = printResult' xs
printResult (Left s) = show s

printResult' [] = ""
printResult' (Right x:xs) = show x ++ "\n" ++ printResult' xs
printResult' (Left x:xs) = show x ++ "\n" ++ printResult' xs

main :: IO ()
main = do
  input <- getContents
  print $ stringToProgram input
  print $ programToExVal `liftM` stringToProgram input
  putStrLn $ printResult $ programToExVal `liftM` stringToProgram input

