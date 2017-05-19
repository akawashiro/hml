module Eval where

import Parse
import Data.Maybe

data ExVal = VInt Int | VBool Bool | VProc String Expr [(String,DnVal)] deriving Show
type DnVal = ExVal

type Env = [(String, ExVal)]

programToExVal :: Program -> [Either String ExVal]
programToExVal = map (exprToExVal [])

exprToExVal :: Env -> Expr -> Either String ExVal
exprToExVal _ (EInt x) = Right $ VInt x
exprToExVal _ (EBool x) = Right $ VBool x
exprToExVal env (EBinOp o x y) = do 
  VInt xx <- exprToExVal env x
  VInt yy <- exprToExVal env y
  case o of
    Plus -> Right $ VInt (xx+yy)
    Mult -> Right $ VInt (xx*yy)
    Lt   -> Right $ VBool (xx<yy)
exprToExVal env (EVariable s) = 
  if isNothing (lookup s env) 
  then Left "Cannot lookup variable."
  else Right $ fromJust (lookup s env) 
exprToExVal env (ELet s e1 e2) = do
  v <- exprToExVal env e1
  exprToExVal ((s,v):env) e2
exprToExVal env (EFun s e) = Right $ VProc s e env
exprToExVal env (EApp e1 e2) = do
  f <- exprToExVal env e1
  a <- exprToExVal env e2
  case f of
    VProc s e env' -> exprToExVal ((s,a):env') e
    _ -> Left "Non-functional expression cannot be applyed."
exprToExVal env (ELetRec s1 s2 e1 e2) = 
  exprToExVal a e2
  where
    a = (s1,VProc s2 e1 a):env
exprToExVal env (EIf e1 e2 e3) = do
  v <- exprToExVal env e1
  case v of
    VBool b -> if b then exprToExVal env e2 else exprToExVal env e3

exprToExVal _  _ = Left "Not implemented."
