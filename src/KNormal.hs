module KNormal where

import qualified Parse as P
import Control.Monad.State
import Data.Maybe

newtype Var = Var String
instance Show Var where
  show (Var s) = s

data Op = OLess | OPlus | OMinus | OTimes
instance Show Op where
  show OLess = "<"
  show OPlus = "+"
  show OMinus = "-"
  show OTimes = "*"

data Exp = EInt Integer | 
           EOp Op Exp Exp |
           EIf Exp Exp Exp |
           ELet Var Exp Exp |
           EVar Var |
           ERec Var [Var] Exp Exp |
           EApp Exp [Exp]
instance Show Exp where
  show (EInt i) = show i
  show (EOp o e1 e2) = "(" ++ show o ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ "\nthen " ++ show e2 ++ "\nelse " ++ show e3
  show (ELet v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in\n" ++ show e2
  show (EVar v) = show v
  show (ERec x ys e1 e2) = "let rec " ++ show x ++ " " ++ show ys ++ " = " ++ show e1 ++ " in\n" ++ show e2
  show (EApp e1 e2s) = show e1 ++ " " ++ show e2s

exprToKNormalExpr :: P.Exp -> Exp
exprToKNormalExpr exp = evalState (exprToKNormalExpr' exp) 0

generateNewName :: State Int Var
generateNewName = do
  i <- get
  put $ i + 1
  return $ Var ("_tmp_" ++ show i)

exprToKNormalExpr' :: P.Exp -> State Int Exp
exprToKNormalExpr' exp = case exp of
  P.EInt i -> return $ EInt i
    -- s <- generateNewName
    -- return $ ELet s (EInt i) (EVar s)
  P.EBool True -> return $ EInt 1
  P.EBool False -> return $ EInt 0
  P.EOp o e1 e2 -> do
    s1 <- generateNewName
    s2 <- generateNewName
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ELet s1 e1' (ELet s2 e2' (EOp (f o) (EVar s1) (EVar s2)))
      where f P.OLess = OLess
            f P.OPlus = OPlus
            f P.OMinus = OMinus
            f P.OTimes = OTimes
  P.EIf e1 e2 e3 -> do
    s1 <- generateNewName
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    e3' <- exprToKNormalExpr' e3
    return $ ELet s1 e1' (EIf (EVar s1) e2' e3')
  P.ELet (P.Var s) e1 e2 -> do
    s1 <- generateNewName
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ELet s1 e1' (ELet (Var s) (EVar s1) e2')
  P.EApp e1 e2s -> do
    e1' <- exprToKNormalExpr' e1
    as <- mapM f e2s
    return (g as (EApp e1' (map (EVar . fst) as)))
      where f e = do
              e' <- exprToKNormalExpr' e
              s <- generateNewName
              return (s, e')
            g [] k = k
            g ((s,e):rest) k = (ELet s e (g rest k))
  P.ERec (P.Var s1) ys e1 e2 -> do
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ERec (Var s1) (map f ys) e1' e2'
      where f (P.Var s) = Var s
  P.EVar (P.Var s) -> return $ EVar (Var s)
