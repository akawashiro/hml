module KNormal where

import Parse
import Control.Monad.State
import Data.Maybe

programToKNormalProgram :: Program -> Program
programToKNormalProgram = map exprToKNormalExpr

exprToKNormalExpr :: Expr -> Expr
exprToKNormalExpr exp = evalState (exprToKNormalExpr' exp) 0

generateNewName :: State Int String
generateNewName = do
  i <- get
  put $ i + 1
  return $ "_tmp_" ++ show i

appExprToKNormalExpr :: Expr -> State Int Expr
appExprToKNormalExpr exp = case exp of
  EApp e1 e2 -> do
    e1' <- appExprToKNormalExpr e1
    e2' <- appExprToKNormalExpr e2
    return $ EApp e1' e2'
  _ -> exprToKNormalExpr' exp

exprToKNormalExpr' :: Expr -> State Int Expr 
exprToKNormalExpr' exp = case exp of
  EInt i -> do 
    s <- generateNewName
    return $ ELet s (EInt i) (EVariable s)
  -- EBool b -> return $ EBool b
  EBinOp o e1 e2 -> do
    s <- generateNewName
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ELet s (EBinOp o e1' e2') (EVariable s)
  EIf e1 e2 e3 -> do
    s1 <- generateNewName
    s2 <- generateNewName
    s3 <- generateNewName
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    e3' <- exprToKNormalExpr' e3
    return $ ELet s1 e1' (EIf (EVariable s1) (ELet s2 e2' (EVariable s2)) (ELet s3 e3' (EVariable s3)))
  ELet s e1 e2 -> do
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ELet s e1' e2'
  EFun s e -> do
    e' <- exprToKNormalExpr' e
    return $ EFun s e'
  EApp e1 e2 -> do
    s <- generateNewName
    e1' <- appExprToKNormalExpr e1
    e2' <- appExprToKNormalExpr e2
    return $ ELet s (EApp e1' e2') (EVariable s)
  ELetRec s1 s2 e1 e2 -> do
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ELetRec s1 s2 e1' e2'
  EVariable s -> return $ EVariable s
