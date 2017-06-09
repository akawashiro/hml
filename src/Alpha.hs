module Alpha where

import Parse
import Control.Monad.State
import qualified Data.Map as Map

exprToAlphaExpr :: Expr -> Expr
exprToAlphaExpr exp = evalState (exprToAlphaExpr' exp) Map.empty

addNewName :: Map.Map -> String -> Map.Map
addNewName m s = if Map.member s m then m else Map.insert m (s ++ show $ Map.size m)

exprToAlphaExpr' :: Expr -> State (Map.Map string string) Expr 
exprToAlphaExpr' exp = case exp of
  EInt i -> return EInt i
  EBool b -> return EBool b
  EIf e1 e2 e3 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    e3' <- exprToAlphaExpr' e3
    return (EIf e1' e2' e3')
  ELet s e1 e2 -> do
    m 
