module Alpha where

-- Alpha conversion and convert Let function defintion into LetRec defintion.

import Parse
import Control.Monad.State
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

-- programToAlphaProgram :: Program -> Program
-- programToAlphaProgram = map exprToAlphaExpr

exprToAlphaExpr :: Expr -> Expr
exprToAlphaExpr exp = evalState (exprToAlphaExpr' exp) (Map.empty,0)

type NameState = (Map.Map String String, Int)

addNewName :: String -> String -> State NameState ()
addNewName prefix s = do
  (m,i) <- get
  unless (Map.member s m) $ put (Map.insert s (prefix ++ s ++ "_" ++ show i) m , i+1)

rename :: String -> State NameState String
rename s = do
  (m,_) <- get
  return $ maybe ("Cannot find name of " ++ show s) id (Map.lookup s m)

isFun :: Expr -> Bool
isFun exp = case exp of
  EFun _ _ -> True
  ELetRec _ _ _ _ -> True
  ELet _ _ e -> isFun e
  _ -> False

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
  ELet s1 (EFun s2 e1) e2 -> do
    addNewName "fun_" s1
    s1' <- rename s1
    e2' <- exprToAlphaExpr' e2
    addNewName "val_" s2
    s2' <- rename s2
    e1' <- exprToAlphaExpr' e1
    return $ ELetRec s1' s2' e1' e2'
  ELet s e1 e2 -> do
    addNewName "val_" s
    s' <- rename s
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ ELet s' e1' e2'
  EFun s e -> do
    addNewName "val_" s
    s' <- rename s
    e' <- exprToAlphaExpr' e
    return $ EFun s' e'
  EApp e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ EApp e1' e2'
  ELetRec s1 s2 e1 e2 -> do
    addNewName "fun_" s1
    s1' <- rename s1
    e2' <- exprToAlphaExpr' e2
    addNewName "val_" s2
    s2' <- rename s2
    e1' <- exprToAlphaExpr' e1
    return $ ELetRec s1' s2' e1' e2'
  EVariable s -> do
    s' <- rename s
    return $ EVariable s'

