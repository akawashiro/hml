{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Closure where

import qualified KNormal as K
import Data.List

data Exp = EInt Integer | 
           EOp K.Op Exp Exp |
           EIf Exp Exp Exp |
           ELet K.Var Exp Exp |
           EDTuple [K.Var] Exp Exp |
           EVar K.Var |
           ERec K.Var [K.Var] Exp Exp |
           EAppCls Exp [Exp] |
           ETuple [Exp]
           deriving (Eq)
instance Show Exp where
  show (EInt i) = show i
  show (EOp o e1 e2) = "(" ++ show o ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ "\nthen " ++ show e2 ++ "\nelse " ++ show e3
  show (ELet v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in\n" ++ show e2
  show (EDTuple vs e1 e2) = "let (" ++ concat (intersperse ", " (map show vs)) ++ ") = " ++ show e1 ++ " in\n" ++ show e2
  show (EVar v) = show v
  show (ERec x ys e1 e2) = "let rec " ++ show x ++ " " ++ show ys ++ " =\n" ++ show e1 ++ " in\n" ++ show e2
  show (EAppCls e1 e2s) = show e1 ++ " " ++ show e2s
  show (ETuple es) = "(" ++ concat (intersperse ", " (map show es)) ++ ")"

adder :: K.Exp
adder = (K.ERec (K.Var "make_adder") [(K.Var "x")]
          (K.ERec (K.Var "adder") [(K.Var "y")] 
            (K.EOp K.OPlus (K.EVar (K.Var "x")) (K.EVar (K.Var "y"))) (K.EApp (K.EVar (K.Var "adder")) [(K.EInt 3)]))
          (K.EApp (K.EVar (K.Var "make_adder")) [(K.EInt 7)]))

adder2 :: K.Exp
adder2 = (K.ERec 
          (K.Var "make_adder") [(K.Var "x"), (K.Var "y")] 
          (K.EOp K.OPlus (K.EVar (K.Var "x")) (K.EVar (K.Var "y"))) 
          (K.EApp (K.EVar (K.Var "make_adder")) [(K.EInt 7), (K.EInt 10)]))

fv :: K.Exp -> [K.Var]
fv (K.EInt _) = []
fv (K.EOp _ e1 e2) = fv e1 ++ fv e2
fv (K.EIf e1 e2 e3) = fv e1 ++ fv e2 ++ fv e3
fv (K.ELet v e1 e2) = (fv e1 ++ fv e2) `lminus` [v]
fv (K.EVar v) = [v]
fv (K.ERec x ys e1 e2) = (fv e1 ++ fv e2) `lminus` (x:ys)
fv (K.EDTuple xs e1 e2) = (fv e1 ++ fv e2) `lminus` xs
fv (K.EApp e1 e2) = (fv e1 ++ concat (map fv e2))
fv (K.ETuple e) = concat (map fv e)

lminus :: Eq a => [a] -> [a] -> [a]
lminus xs ys = filter (\x -> not (elem x ys)) xs
