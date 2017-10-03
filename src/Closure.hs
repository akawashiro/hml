module Closure where

import Parse
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

-- programToClosureProgram :: Program -> Program
-- programToClosureProgram = map exprToClosureExpr

-- exp -> known variables -> free variables
exprToFreeVariables :: Expr -> [String] -> [String]
exprToFreeVariables exp boundedVariables = case exp of
  EInt i -> []
  EBool b -> []
  EVariable v -> if v `elem` boundedVariables || take 3 v == "fun" then [] else [v]
  EBinOp o e1 e2 ->
    let v1 = exprToFreeVariables e1 boundedVariables in
    let v2 = exprToFreeVariables e2 boundedVariables in
    v1 ++ v2
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
exprToClosureFun :: Expr -> [String] -> Expr
exprToClosureFun exp [] = exp
exprToClosureFun exp (v:vs) = exprToClosureFun (EFun v exp) vs

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
  EBinOp o e1 e2 -> do
    e1' <- exprToClosureExpr' e1
    e2' <- exprToClosureExpr' e2
    return $ EBinOp o e1' e2'
  EIf e1 e2 e3 -> do
    e1' <- exprToClosureExpr' e1
    e2' <- exprToClosureExpr' e2
    e3' <- exprToClosureExpr' e3
    return $ EIf e1' e2' e3'
  EApp e1 e2 -> do
    e1' <- exprToClosureExpr' e1
    e2' <- exprToClosureExpr' e2
    return $ EApp e1' e2'
  EFun s e -> do
    e' <- exprToClosureExpr' e
    return $ EFun s e'
  ELet s e1 e2 -> do
    e1' <- exprToClosureExpr' e1
    e2' <- exprToClosureExpr' e2
    return $ ELet s e1' e2'
  ELetRec s1 s2 e1 e2 -> do
    m <- get
    put $ Map.insert s1 (reverse (tail fs)) m
    e1' <- exprToClosureExpr' (exprToClosureFun e1 (cutLast1 fs))
    e2' <- exprToClosureExpr' e2
    return $ ELetRec s1 (last fs) e1' e2'
      where
        fs = s2:filter (\x -> x /= s2 && x/=s1) (exprToFreeVariables e1 [])
        cutLast1 [] = []
        cutLast1 [a] = []
        cutLast1 (v:vs) = v:cutLast1 vs
