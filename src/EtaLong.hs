module EtaLong where

import Parse
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

type S = State (Map.Map String Int,Int)

getFreshFunName :: S String
getFreshFunName = do
  (_,i) <- get
  return $ "_fun_" ++ show i

getFreshArgName :: S String
getFreshArgName = do
  (_,i) <- get
  return $ "_arg_" ++ show i

putNumberOfArgs :: Expr -> S ()
putNumberOfArgs exp = do
  (m,i) <- get
  let (fn,na) = numberOfArgs exp
  put (Map.insert fn na m,i)

numberOfArgs :: Expr -> (String,Int)
numberOfArgs (ELetRec s1 s2 e1 e2) = 1 + numberOfArgs e1
numberOfArgs (EFun s e) = 1 + numberOfArgs e
numberOfArgs _ = 0

appToEtaLongApp :: Expr -> Int -> S Expr
appToEtaLongApp exp 0 = return exp
appToEtaLongApp exp 1 = do
  a <- getFreshArgName
  f <- getFreshFunName
  return (ELetRec f a exp (EApp exp (EVariable a)))
appToEtaLongApp exp n = do
  a <- getFreshArgName
  appToEtaLongApp (EFun a (EApp exp (EVariable a))) n-1

-- This function will be failed when it applyed to a function which is renamed by let (not let rec)
appToFunName :: Expr -> String
appToFunName (EVariable s) = s
appToFunName (EApp e1 e2) = appToFunName e1

appToNumberOfAppliedArgs :: Expr -> Int
appToNumberOfAppliedArgs EVariable s = 0
appToNumberOfAppliedArgs EApp e1 e2 = 1 + appToNumberOfAppliedArgs e1

exprToEtaLongExpr :: Expr -> S Expr
exprToEtaLongExpr exp = case exp of
  ELetRec s1 s2 e1 e2 -> do
    putNumberOfArgs exp
    return exp
  EApp e1 e2 -> do
    (m,_) <- get
    let fn = appToFunName exp
    let na = fromJust $ Map.lookup fn m
    appToEtaLongApp exp (na - appToNumberOfAppliedArgs exp)
  _ -> return exp
