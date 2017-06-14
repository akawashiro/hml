module Closure where

import Parse
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

-- exp -> known variables -> free variables
exprToFreeVariables :: Expr -> [String] -> [String]
exprToFreeVariables exp boundedVariables = case exp of
  EInt i -> []
  EBool b -> []
  EVariable v -> if v `elem` boundedVariables then [] else [v]
  EIf e1 e2 e3 ->
    let v1 = exprToFreeVariables e1 boundedVariables in
    let v2 = exprToFreeVariables e2 boundedVariables in
    let v3 = exprToFreeVariables e3 boundedVariables in
    v1 ++ v2 ++ v3
  ELet s e1 e2 ->
    let v1 = exprToFreeVariables e1 boundedVariables in
    let v2 = exprToFreeVariables e2 (s:boundedVariables) in
    v1 ++ v2
  EFun s e ->
    let v = exprToFreeVariables e (s:boundedVariables) in
    v
  EApp e1 e2 ->
    let v1 = exprToFreeVariables e1 boundedVariables in
    let v2 = exprToFreeVariables e2 boundedVariables in
    v1 ++ v2
  ELetRec s1 s2 e1 e2 ->
    let v1 = exprToFreeVariables e1 (s1:s2:boundedVariables) in
    let v2 = exprToFreeVariables e2 (s1:boundedVariables) in
    v1 ++ v2

exprToClosureExpr :: Expr -> Expr
exprToClosureExpr exp = evalState (exprToClosureExpr' exp) Map.empty

-- exp -> free variables in exp -> closure form exp
funToClosureFun :: Expr -> [String] -> Expr
funToClosureFun (EFun s e) vs = EFun s (funToClosureFun e vs)
funToClosureFun exp [] = exp
funToClosureFun exp (v:vs) = funToClosureFun (EFun v exp) vs

callToClosureCall :: Expr -> [String] -> Expr
callToClosureCall exp [] = exp
callToClosureCall exp (v:vs) = callToClosureCall (EApp exp (EVariable v)) vs

exprToClosureExpr' :: Expr -> State (Map.Map String [String]) Expr
exprToClosureExpr' exp = case exp of
  EInt i -> return $ EInt i
  EBool b -> return $ EBool b
  EVariable s -> do
    m <- get
    case Map.lookup s m of
      Nothing -> return (EVariable s)
      Just vs -> return $ callToClosureCall (EVariable s) vs
  EIf e1 e2 e3 -> do
    e1' <- exprToClosureExpr' e1
    e2' <- exprToClosureExpr' e2
    e3' <- exprToClosureExpr' e3
    return $ EIf e1' e2' e3'
  EApp e1 e2 -> do
    e1' <- exprToClosureExpr' e1
    e2' <- exprToClosureExpr' e2
    return $ EApp e1' e2'
  ELet s e1 e2 -> do
    m <- get
    put $ Map.insert s (exprToFreeVariables e1) m
    e1' <- exprToClosureExpr' e1
    e2' <- exprToClosureExpr' e2
    return $ ELet s e1' e2'
  ELetRec s1 s2 e1 e2 -> do
    m <- get
    put $ Map.insert s1 (filter (/= s2) exprToFreeVariables e1) m
    e1' <- exprToClosureExpr' e1
    e2' <- exprToClosureExpr' e2
    return $ ELetRec s1 s2 e1 e2
