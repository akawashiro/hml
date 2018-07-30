module KNormal where

import qualified Parse as P
import Control.Monad.State
import Data.Maybe

newtype Var = Var String deriving (Show)

data Op = OLess | OPlus | OMinus | OTimes deriving (Show)

data Exp = EInt Integer | 
           EBool Bool |
           EOp Op Exp Exp |
           EIf Exp Exp Exp |
           ELet Var Exp Exp |
           EVar Var |
           ERec Var [Var] Exp Exp |
           EApp Exp [Exp]
           deriving (Show)

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
    s <- generateNewName
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ELet s (EOp (f o) e1' e2') (EVar s)
      where f P.OLess = OLess
            f P.OPlus = OPlus
            f P.OMinus = OMinus
            f P.OTimes = OTimes
  P.EIf e1 e2 e3 -> do
    s1 <- generateNewName
    s2 <- generateNewName
    s3 <- generateNewName
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    e3' <- exprToKNormalExpr' e3
    return $ ELet s1 e1' (EIf (EVar s1) (ELet s2 e2' (EVar s2)) (ELet s3 e3' (EVar s3)))
  P.ELet (P.Var s) e1 e2 -> do
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ELet (Var s) e1' e2'
  P.EApp e1 e2 -> do
    s <- generateNewName
    e1' <- exprToKNormalExpr' e1
    e2' <- mapM exprToKNormalExpr' e2
    return $ ELet s (EApp e1' e2') (EVar s)
  P.ERec (P.Var s1) ys e1 e2 -> do
    e1' <- exprToKNormalExpr' e1
    e2' <- exprToKNormalExpr' e2
    return $ ERec (Var s1) (map f ys) e1' e2'
      where f (P.Var s) = Var s
  P.EVar (P.Var s) -> return $ EVar (Var s)
