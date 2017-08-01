module Main where

import Parse
import Eval
import Control.Monad
import Alpha
import KNormal
import Closure
import Flat
import Declare
import Call
import RegisterAllocate

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "input = \n" ++ input ++ "\n"

  let parsed = stringToExpr input
  putStrLn $ "after parsing = \n" ++ show parsed ++ "\n"

  let alphad = exprToAlphaExpr `liftM` parsed
  putStrLn $ "after alpha conversion = \n" ++ show alphad ++ "\n"

  let closured = exprToClosureExpr `liftM` alphad
  putStrLn $ "after closure translation = \n" ++ show closured ++ "\n"

  let knormaled = exprToKNormalExpr `liftM` closured
  putStrLn $ "after KNormal = \n" ++ show knormaled ++ "\n"

  let flatted = exprToFlatExpr `liftM` knormaled
  putStrLn $ "after flatting = \n" ++ show flatted ++ "\n"

  let ists = exprToInstructionList `liftM` flatted
  putStrLn $ "Instructions = \n" ++ show ists

  let ists' = exprToDeclareList `liftM` flatted
  putStrLn $ "Declares = \n" ++ show ists'

  let called = (liftM (liftM (liftM processCall))) ists'
  putStrLn $ "After call normalization = \n" ++ show called

  let alloced = (liftM (liftM (liftM allocate))) called
  putStrLn $ "After allocation = \n" ++ show alloced

  let results = exprToExVal [] `liftM` flatted
  putStrLn "\nresults ="
  print results


