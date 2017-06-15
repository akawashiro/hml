module Main where

import Parse
import Eval
import Control.Monad
import Alpha
import KNormal
import Closure

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "input = \n" ++ input ++ "\n"
  let parsed = stringToProgram input
  putStrLn $ "after parsing = \n" ++ show parsed ++ "\n"
  let closured' = programToClosureProgram `liftM` parsed
  putStrLn $ "after closre translation = \n" ++ show closured' ++ "\n"
  let results = programToExVal `liftM` closured'
  print results
  let knormaled = programToKNormalProgram `liftM` parsed
  putStrLn $ "after KNormal = \n" ++ show knormaled ++ "\n"
  let alphad = programToAlphaProgram `liftM` knormaled
  putStrLn $ "after alpha conversion = \n" ++ show alphad ++ "\n"
  let closured = programToClosureProgram `liftM` alphad
  putStrLn $ "after closre translation = \n" ++ show closured ++ "\n"
  let results = programToExVal `liftM` alphad
  print results
