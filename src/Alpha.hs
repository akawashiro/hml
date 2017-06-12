module Alpha where

import Parse
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

programToAlphaProgram :: Program -> Program
programToAlphaProgram p = map exprToAlphaExpr p

exprToAlphaExpr :: Expr -> Expr
exprToAlphaExpr exp = evalState (exprToAlphaExpr' exp) (Map.empty,0)

type NameState = (Map.Map String String, Int)

addNewName :: String -> State NameState ()
addNewName s = do
  (m,i) <- get
  if Map.member s m
  then return ()
  else put $ (Map.insert s (s ++ "_" ++ show i) m , i+1)

rename :: String -> State NameState String
rename s = do
  (m,_) <- get
  return $ fromJust $ Map.lookup s m

exprToAlphaExpr' :: Expr -> State NameState Expr 
exprToAlphaExpr' exp = case exp of
  EInt i -> return $ EInt i
  EBool b -> return $ EBool b
  EBinOp o e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ EBinOp o e1' e2'
  EIf e1 e2 e3 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    e3' <- exprToAlphaExpr' e3
    return $ EIf e1' e2' e3'
  ELet s e1 e2 -> do
    addNewName s
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    s' <- rename s
    return $ ELet s' e1' e2'
  EFun s e -> do
    addNewName s
    s' <- rename s
    e' <- exprToAlphaExpr' e
    return $ EFun s' e'
  EApp e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ EApp e1' e2'
  ELetRec s1 s2 e1 e2 -> do
    addNewName s1
    s1' <- rename s1
    e2' <- exprToAlphaExpr' e2
    addNewName s2
    s2' <- rename s2
    e1' <- exprToAlphaExpr' e1
    return $ ELetRec s1' s2' e1' e2'
  EVariable s -> do
    s' <- rename s
    return $ EVariable s'

