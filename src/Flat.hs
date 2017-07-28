module Flat where

import Parse

-- programToFlatProgram :: Program -> Program
-- programToFlatProgram = map exprToFlatExpr

exprToFlatExpr :: Expr -> Expr
exprToFlatExpr exp =
  exprToLecFlat $ (exprToPrefixLetRec exp) (exprToSuffixLetRec exp)

exprToPrefixLetRec :: Expr -> (Expr -> Expr)
exprToPrefixLetRec exp = case exp of
  EInt i -> id
  EBool b -> id
  EVariable v -> id
  EBinOp o e1 e2 -> (exprToPrefixLetRec e1) . (exprToPrefixLetRec e2)
  EIf e1 e2 e3 -> (exprToPrefixLetRec e1) . (exprToPrefixLetRec e2) . (exprToPrefixLetRec e3)
  EFun s e -> exprToPrefixLetRec e
  EApp e1 e2 -> (exprToPrefixLetRec e1) . (exprToPrefixLetRec e2)
  ELet s e1 e2 -> (exprToPrefixLetRec e1) . (exprToPrefixLetRec e2)
  ELetRec s1 s2 e1 e2 -> (exprToPrefixLetRec e1) .  (\x -> ELetRec s1 s2 (exprToSuffixLetRec e1) x) . (exprToPrefixLetRec e2)

exprToSuffixLetRec :: Expr -> Expr
exprToSuffixLetRec exp = case exp of
  EInt i -> EInt i
  EBool b -> EBool b
  EVariable v -> EVariable v
  EBinOp o e1 e2 -> EBinOp o (exprToSuffixLetRec e1) (exprToSuffixLetRec e2)
  EIf e1 e2 e3 -> EIf (exprToSuffixLetRec e1) (exprToSuffixLetRec e2) (exprToSuffixLetRec e3)
  EFun s e -> EFun s (exprToSuffixLetRec e)
  EApp e1 e2 -> EApp (exprToSuffixLetRec e1) (exprToSuffixLetRec e2)
  ELet s e1 e2 -> ELet s (exprToSuffixLetRec e1) (exprToSuffixLetRec e2)
  ELetRec s1 s2 e1 e2 -> exprToSuffixLetRec e2

exprToLecFlat :: Expr -> Expr
exprToLecFlat exp = case exp of
  EInt i -> EInt i
  EBool b -> EBool b
  EVariable v -> EVariable v
  EBinOp o e1 e2 -> EBinOp o (exprToLecFlat e1) (exprToLecFlat e2)
  EIf e1 e2 e3 -> EIf (exprToLecFlat e1) (exprToLecFlat e2) (exprToLecFlat e3)
  EFun s e -> EFun s (exprToLecFlat e)
  EApp e1 e2 -> EApp (exprToLecFlat e1) (exprToLecFlat e2)
  ELet s e1 e2 -> (exprToPrefixLet e1) (ELet s (exprToSuffixLet e1) (exprToLecFlat e2))
  ELetRec s1 s2 e1 e2 -> ELetRec s1 s2 (exprToFlatExpr e1) (exprToFlatExpr e2)

exprToPrefixLet :: Expr -> (Expr -> Expr)
exprToPrefixLet exp = case exp of
  EInt i -> id
  EBool b -> id
  EVariable v -> id
  EBinOp o e1 e2 -> (exprToPrefixLet e1) . (exprToPrefixLet e2)
  EIf e1 e2 e3 -> (exprToPrefixLet e1) . (exprToPrefixLet e2) . (exprToPrefixLet e3)
  EFun s e -> exprToPrefixLet e
  EApp e1 e2 -> (exprToPrefixLet e1) . (exprToPrefixLet e2)
  ELet s e1 e2 -> (exprToPrefixLet e1) . (\x -> ELet s (exprToSuffixLet e1) x) . (exprToPrefixLet e2)
  ELetRec s1 s2 e1 e2 -> (exprToPrefixLetRec e1) . (exprToPrefixLetRec e2)

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
  ELetRec s1 s2 e1 e2 -> ELetRec s1 s2 (exprToSuffixLet e1) (exprToSuffixLet e2)



