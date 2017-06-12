module Main where

import Parse
import Eval
import Control.Monad
import Alpha
import KNormal

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "input = \n" ++ input ++ "\n"
  let parsed = stringToProgram input
  putStrLn $ "after parsing = \n" ++ show parsed ++ "\n"
  let knormaled = programToKNormalProgram `liftM` parsed
  putStrLn $ "after KNormal = \n" ++ show knormaled ++ "\n"
  let alphad = programToAlphaProgram `liftM` knormaled
  putStrLn $ "after alpha conversion = \n" ++ show alphad ++ "\n"
  let results = programToExVal `liftM` alphad
  print results
