module Main where

import Parse
import Eval
import Control.Monad
import Alpha
import KNormal
import Closure
import Flat

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "input = \n" ++ input ++ "\n"

  let parsed = stringToProgram input
  putStrLn $ "after parsing = \n" ++ show parsed ++ "\n"

  let alphad = programToAlphaProgram `liftM` parsed
  putStrLn $ "after alpha conversion = \n" ++ show alphad ++ "\n"

  let knormaled = programToKNormalProgram `liftM` alphad
  putStrLn $ "after KNormal = \n" ++ show knormaled ++ "\n"

  let closured = programToClosureProgram `liftM` knormaled
  putStrLn $ "after closure translation = \n" ++ show closured ++ "\n"

  let flatted = programToFlatProgram `liftM` closured
  putStrLn $ "after flatting = \n" ++ show flatted ++ "\n"

  let results = programToExVal `liftM` flatted
  print results
