module EtaLong where

import Parse
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

type S = State (Map.Map String Int,Int)

getFreshFunName :: S String
getFreshFunName = do
  (m,i) <- get
  put (m,i+1)
  return $ "_fun_" ++ show i

getFreshArgName :: S String
getFreshArgName = do
  (m,i) <- get
  put (m,i+1)
  return $ "_arg_" ++ show i

putNumberOfArgs :: Expr -> S ()
putNumberOfArgs exp = do
  (m,i) <- get
  let (fn,na) = (nameOfLet exp,numberOfArgs exp)
  put (Map.insert fn na m,i)

nameOfLet (ELetRec s1 s2 e1 e2) = s1
nameOfLet (ELet s e1 e2) = s
nameOfLet _ = "This is not let rec function."

numberOfArgs :: Expr -> Int
numberOfArgs (ELetRec s1 s2 e1 e2) = 1 + numberOfArgs e1
numberOfArgs (ELet s1 (EFun s2 e1) e2) = 1 + numberOfArgs e1
numberOfArgs (EFun s e) = 1 + numberOfArgs e
numberOfArgs _ = 0

appToEtaLongApp :: Expr -> Int -> S Expr
appToEtaLongApp exp (-1) = return exp
appToEtaLongApp exp (-2) = return exp
appToEtaLongApp exp 0 = return exp
appToEtaLongApp exp n = do
  a <- getFreshArgName
  appToEtaLongApp (EFun a (EApp exp (EVariable a))) (n-1)

-- This function will be failed when it applyed to a function which is renamed by let (not let rec)
appToFunName :: Expr -> String
appToFunName (EVariable s) = s
appToFunName (EApp e1 e2) = appToFunName e1

appToNumberOfAppliedArgs :: Expr -> Int
appToNumberOfAppliedArgs (EVariable s) = 0
appToNumberOfAppliedArgs (EApp e1 e2) = 1 + appToNumberOfAppliedArgs e1

exprToEtaLongExpr :: Expr -> Expr
exprToEtaLongExpr exp = evalState (exprToEtaLongExpr' exp) (Map.empty,0)

exprToEtaLongExpr' :: Expr -> S Expr
exprToEtaLongExpr' exp = case exp of
  ELetRec s1 s2 e1 e2 -> do
    e1' <- exprToEtaLongExpr' e1
    putNumberOfArgs (ELetRec s1 s2 e1' e2)
    e2' <- exprToEtaLongExpr' e2
    return (ELetRec s1 s2 e1' e2')
  EApp e1 e2 -> do
    (m,_) <- get
    let fn = appToFunName exp
    let na = maybe 4 id (Map.lookup fn m)
    appToEtaLongApp exp (na - appToNumberOfAppliedArgs exp)
  ELet s1 e1 e2 -> do
    e1' <- exprToEtaLongExpr' e1
    putNumberOfArgs (ELet s1 e1' e2)
    e2' <- exprToEtaLongExpr' e2
    return $ ELet s1 e1' e2'
  EBinOp op e1 e2 -> do
    e1' <- exprToEtaLongExpr' e1
    e2' <- exprToEtaLongExpr' e2
    return $ EBinOp op e1' e2'
  EVariable s -> return $ exp
  EFun s e -> do
    e' <- exprToEtaLongExpr' e
    return $ EFun s e'
  EIf e1 e2 e3 -> do
    e1' <- exprToEtaLongExpr' e1
    e2' <- exprToEtaLongExpr' e2
    e3' <- exprToEtaLongExpr' e3
    return $ EIf e1' e2' e3'
  EInt i -> return exp
  -- _ -> exprToEtaLongExpr' exp
