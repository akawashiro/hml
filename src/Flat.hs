module Flat where

import Parse

programToFlatProgram :: Program -> Program
programToFlatProgram = map exprToFlatExpr

exprToFlatExpr :: Expr -> Expr
exprToFlatExpr exp =
  (exprToPrefixLet exp) (exprToSuffixLet exp)

exprToPrefixLet :: Expr -> (Expr -> Expr)
exprToPrefixLet exp = case exp of
  EInt i -> id
  EBool b -> id
  EVariable v -> id
  EBinOp o e1 e2 -> (exprToPrefixLet e1) . (exprToPrefixLet e2)
  EIf e1 e2 e3 -> (exprToPrefixLet e1) . (exprToPrefixLet e2) . (exprToPrefixLet e3)
  EFun s e -> exprToPrefixLet e
  EApp e1 e2 -> (exprToPrefixLet e1) . (exprToPrefixLet e2)
  ELet s e1 e2 -> (exprToPrefixLet e1) .  (\x -> ELet s (exprToSuffixLet e1) x) . (exprToPrefixLet e2)
  ELetRec s1 s2 e1 e2 -> (exprToPrefixLet e1) .  (\x -> ELetRec s1 s2 (exprToSuffixLet e1) x) . (exprToPrefixLet e2)

exprToSuffixLet :: Expr -> Expr
exprToSuffixLet exp = case exp of
  EInt i -> EInt i
  EBool b -> EBool b
  EVariable v -> EVariable v
  EBinOp o e1 e2 -> EBinOp o (exprToSuffixLet e1) (exprToSuffixLet e2)
  EIf e1 e2 e3 -> EIf (exprToSuffixLet e1) (exprToSuffixLet e2) (exprToSuffixLet e3)
  EFun s e -> EFun s (exprToSuffixLet e)
  EApp e1 e2 -> EApp (exprToSuffixLet e1) (exprToSuffixLet e2)
  ELet s e1 e2 -> exprToSuffixLet e2
  ELetRec s1 s2 e1 e2 -> exprToSuffixLet e2

