{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Closure where

import qualified KNormal as K
import Data.List
import Control.Monad.State

data Var = Var String | Label String deriving (Eq)
instance Show Var where
  show (Var s) = s
  show (Label s) = s

data Exp = EInt Integer | 
           EOp K.Op Exp Exp |
           EIf Exp Exp Exp |
           ELet Var Exp Exp |
           EDTuple [Var] Exp Exp |
           EVar Var |
           ERec Var [Var] Exp Exp |
           EAppCls Exp [Exp] |
           ETuple [Exp]
           deriving (Eq)
instance Show Exp where
  show (EInt i) = show i
  show (EOp o e1 e2) = "(" ++ show o ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ "\nthen " ++ show e2 ++ "\nelse " ++ show e3
  show (ELet v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in\n" ++ show e2
  show (EDTuple vs e1 e2) = "let (" ++ intercalate ", " (map show vs) ++ ") = " ++ show e1 ++ " in\n" ++ show e2
  show (EVar v) = show v
  show (ERec x ys e1 e2) = "let rec " ++ show x ++ " " ++ show ys ++ " =\n" ++ show e1 ++ " in\n" ++ show e2
  show (EAppCls e1 e2s) = show e1 ++ " " ++ show e2s
  show (ETuple es) = "(" ++ intercalate ", " (map show es) ++ ")"

data FunDef = FunDef Var [Var] Exp deriving (Eq)
instance Show FunDef where
  show (FunDef x ys e) = "fundef " ++ show x ++ " " ++ intercalate " " (map show ys) ++ " =\n" ++ show e

data Prog = Prog [FunDef] Exp deriving (Eq)
instance Show Prog where
  show (Prog fd exp) = intercalate "\n" (map show fd) ++ "\n" ++ show exp 

type ClsTransM = State [FunDef]

addFunDef :: FunDef -> ClsTransM ()
addFunDef fd = do
  fs <- get
  put (fd:fs)

clsTrans :: K.Exp -> Prog
clsTrans exp = 
  let (e,fd) = runState (clsTrans' exp) [] in
  Prog fd e

clsTrans' :: K.Exp -> ClsTransM Exp
clsTrans' (K.EInt i) = return (EInt i)
clsTrans' (K.EOp o e1 e2) = EOp o <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (K.EIf e1 e2 e3) = EIf <$> clsTrans' e1 <*> clsTrans' e2 <*> clsTrans' e3
clsTrans' (K.ELet (K.Var x) e1 e2) = ELet (Var x) <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (K.EDTuple xs e1 e2) = EDTuple (map v2v xs) <$> clsTrans' e1 <*> clsTrans' e2
clsTrans' (K.EVar (K.Var s)) = return (EVar (Var s))
clsTrans' (K.EApp e1 e2) = EAppCls <$> clsTrans' e1 <*> mapM clsTrans' e2
clsTrans' (K.ETuple es) = ETuple <$> mapM clsTrans' es
clsTrans' (K.ERec x ys e1 e2) = do
  e1' <- clsTrans' e1
  e2' <- clsTrans' e2
  let fvs = fv e1 `lminus` (x:ys)
  let fd = FunDef (v2l x) (v2cls x:map v2v ys) (EDTuple (v2tmp x:map v2v fvs) (EVar (v2cls x)) e1')
  addFunDef fd
  return $ ELet (v2v x) (ETuple (map EVar (v2l x:map v2v fvs))) e2'


v2v :: K.Var -> Var
v2v (K.Var s) = Var s

v2cls :: K.Var -> Var
v2cls (K.Var s) = Var ("cls_" ++ s)

v2tmp :: K.Var -> Var
v2tmp (K.Var s) = Var ("tmp_" ++ s)

v2l :: K.Var -> Var
v2l (K.Var s) = Label ("def_" ++ s)

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
