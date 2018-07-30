module Alpha where

import KNormal (Exp(..), Var(..))
import Control.Monad.State
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

exprToAlphaExpr :: Exp -> Exp
exprToAlphaExpr exp = evalState (exprToAlphaExpr' exp) (Map.empty,0)

type NameState = (Map.Map String String, Int)

addNewName :: String -> Var -> State NameState ()
addNewName prefix (Var s) = do
  (m,i) <- get
  unless (Map.member s m) $ put (Map.insert s (prefix ++ s ++ "_" ++ show i) m , i+1)

rename :: Var -> State NameState Var
rename (Var s) = do
  (m,_) <- get
  return $ maybe (Var $ "Cannot find name of " ++ show s) Var (Map.lookup s m)

isFun :: Exp -> Bool
isFun exp = case exp of
  ERec _ _ _ _ -> True
  ELet _ _ e -> isFun e
  _ -> False

exprToAlphaExpr' :: Exp -> State NameState Exp
exprToAlphaExpr' exp = case exp of
  EInt i -> return $ EInt i
  EOp o e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ EOp o e1' e2'
  EIf e1 e2 e3 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    e3' <- exprToAlphaExpr' e3
    return $ EIf e1' e2' e3'
  ELet s e1 e2 -> do
    addNewName "val_" s
    s' <- rename s
    e1' <- exprToAlphaExpr' e1
    e2' <- exprToAlphaExpr' e2
    return $ ELet s' e1' e2'
  EApp e1 e2 -> do
    e1' <- exprToAlphaExpr' e1
    e2' <- mapM exprToAlphaExpr' e2
    return $ EApp e1' e2'
  ERec s1 s2 e1 e2 -> do
    addNewName "fun_" s1
    s1' <- rename s1
    e2' <- exprToAlphaExpr' e2
    mapM_ (addNewName "val_") s2
    s2' <- mapM rename s2
    e1' <- exprToAlphaExpr' e1
    return $ ERec s1' s2' e1' e2'
  EVar s -> do
    s' <- rename s
    return $ EVar s'
