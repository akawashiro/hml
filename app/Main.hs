module Main where

import Parse
import Eval
import Control.Monad
import Alpha
import KNormal
import Closure
import Flat
import Declare

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "input = \n" ++ input ++ "\n"

  let parsed = stringToProgram input
  putStrLn $ "after parsing = \n" ++ show parsed ++ "\n"

  let alphad = programToAlphaProgram `liftM` parsed
  putStrLn $ "after alpha conversion = \n" ++ show alphad ++ "\n"

  let closured = programToClosureProgram `liftM` alphad
  putStrLn $ "after closure translation = \n" ++ show closured ++ "\n"

  let knormaled = programToKNormalProgram `liftM` closured
  putStrLn $ "after KNormal = \n" ++ show knormaled ++ "\n"

  let flatted = programToFlatProgram `liftM` knormaled
  putStrLn $ "after flatting = \n" ++ show flatted ++ "\n"

  let decls = (map exprToDeclList) `liftM` flatted
  putStrLn $ "Declaration = \n" ++ show decls

  let results = programToExVal `liftM` flatted
  putStrLn "\nresults ="
  print results


