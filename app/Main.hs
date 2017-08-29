{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.Trans
import Options.Declarative
import Parse
-- import Eval
import EtaLong
import Control.Monad
import Alpha
import KNormal
import Closure
import Flat
import Declare
import Call
import Register
import Stack
import Data.Either

genCode :: [[Instruction]] -> String
genCode is = "\t.data\nNL:\n\t.asciiz \"\\n\"\n\t.text\n\t.globl main\n" ++ (concat (map (concat.(map show)) is))

inputToInst :: String -> IO ()
inputToInst input = do
  let parsed = stringToExpr input
  let etad = exprToEtaLongExpr `liftM` parsed
  let alphad = exprToAlphaExpr `liftM` etad
  let closured = exprToClosureExpr `liftM` alphad
  let knormaled = exprToKNormalExpr `liftM` closured
  let flatted = exprToFlatExpr `liftM` knormaled
  let ists = exprToDeclareList `liftM` flatted
  let called = (liftM (liftM (liftM processCall))) ists
  let alloced = (liftM (liftM (liftM allocate))) called
  let stacked = (liftM $ liftM $ liftM processStack) alloced
  putStr $ genCode $ e $ e $ stacked

  -- putStr $ genCode $ e $ e $ (liftM $ liftM $ liftM processStack) $ 
  -- (liftM (liftM (liftM allocate))) $ (liftM (liftM (liftM processCall))) 
  -- exprToDeclareList `liftM` exprToFlatExpr `liftM` exprToKNormalExpr `liftM` 
  -- exprToClosureExpr `liftM` exprToAlphaExpr `liftM` exprToEtaLongExpr `liftM` (stringToExpr input)
  where e (Right x) = x

showDetails :: String -> IO ()
showDetails input = do
  putStrLn $ "Input = \n" ++ input ++ "\n"

  let parsed = stringToExpr input
  putStrLn $ "After parsing = \n" ++ show parsed ++ "\n"

  let etad = exprToEtaLongExpr `liftM` parsed
  putStrLn $ "After eta expansion = \n" ++ show etad ++ "\n"

  let alphad = exprToAlphaExpr `liftM` etad
  putStrLn $ "After alpha conversion = \n" ++ show alphad ++ "\n"

  let closured = exprToClosureExpr `liftM` alphad
  putStrLn $ "After closure translation = \n" ++ show closured ++ "\n"

  let knormaled = exprToKNormalExpr `liftM` closured
  putStrLn $ "After KNormalization = \n" ++ show knormaled ++ "\n"

  let flatted = exprToFlatExpr `liftM` knormaled
  putStrLn $ "After flatting = \n" ++ show flatted ++ "\n"

  let ists = exprToDeclareList `liftM` flatted
  putStrLn $ "Declares = \n" ++ show ists

  let called = (liftM (liftM (liftM processCall))) ists
  putStrLn $ "After call normalization = \n" ++ show called

  let alloced = (liftM (liftM (liftM allocate))) called
  putStrLn $ "After register allocation = \n" ++ show alloced

  let stacked = (liftM $ liftM $ liftM processStack) alloced
  putStrLn $ "After stack allocation = \n" ++ show stacked

compile :: Flag "d" '["debug"] "" "debug option" Bool
        -> Arg "Sorce file" String
        -> Cmd "MiniML compiler" ()
compile debug source = do
  let f | get debug = readFile (get source) >>= showDetails
        | otherwise = readFile (get source) >>= inputToInst
  liftIO f

main :: IO ()
main = run_ compile
