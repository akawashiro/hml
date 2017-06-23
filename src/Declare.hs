module Declare where

import Parse
import Control.Monad.State
import Control.Monad.Trans.Either

data Decl = DInt Int 
            | DBinOp BinOp String String String
            | DBeqz String String -- First operand is condition
            | DOriZ String Int
            | DCall String [String] String
            | DArgs [String]
            | DMove String String
            | DRet String
            | DLabel String
            | DJump String

instance Show Decl where
  show (DInt i)             = "\t" ++ show i ++ "\n"
  show (DBinOp op rs rt rd) = "\t" ++ rd ++ " = " ++ rs ++ " " ++ show op ++ " " ++ rt ++ "\n"
  show (DBeqz c l)          = "\t" ++ "beqz " ++ c ++ " " ++ l ++ "\n"
  show (DOriZ r i)          = "\t" ++ "oriz " ++ r ++ " $0 " ++ show i ++ "\n"
  show (DCall f as rd)      = "\t" ++ rd ++ " = " ++ f ++ " " ++ show as ++ "\n"
  show (DArgs as)           = "\t" ++ "args " ++ show as ++ "\n"
  show (DMove rs rt)        = "\t" ++ "move " ++ rs ++ " " ++ rt ++ "\n"
  show (DRet rs)            = "\t" ++ "ret " ++ rs ++ "\n"
  show (DLabel l)           = l ++ ":" ++ "\n"
  show (DJump l)            = "\t" ++ "jump" ++ l ++ "\n"

exprToDeclList exp = evalState (runEitherT (exprToDeclList' exp)) 0

makeIfLabel :: EitherT String (State Int) String
makeIfLabel = do
  i <- (lift get)
  lift (put (i+1))
  return $ "_if_label_" ++ show i

appToFunAndArgs :: Expr -> EitherT String (State Int) (String,[String])
appToFunAndArgs (EVariable s) = return (s,[])
appToFunAndArgs (EApp e (EVariable s)) = do
  (f,as) <- appToFunAndArgs e
  return (f,s:as)
appToFunAndArgs _ = left "Application expression is not inappropriate."

funToArgsAndBody :: Expr -> EitherT String (State Int) ([String],Expr)
funToArgsAndBody (EFun s e) = do
  (as,b) <- funToArgsAndBody e
  return (s:as,b)
funToArgsAndBody e = return ([],e)

exprToDeclList' :: Expr -> EitherT String (State Int) [Decl]
exprToDeclList' exp = case exp of
  EIf e1 e2 e3 -> case e1 of
    EVariable s -> do
      l1 <- makeIfLabel
      l2 <- makeIfLabel
      dl1 <- exprToDeclList' e1
      dl2 <- exprToDeclList' e2
      return $ [DBeqz s l1] ++ dl1 ++ [DJump l2,DLabel l1] ++ dl2 ++ [DLabel l2]
    EInt i -> do
      dl1 <- exprToDeclList' e1
      dl2 <- exprToDeclList' e2
      return $ if i /= 0 then dl1 else dl2
    _ -> left "If condition is not variable or constant."
  ELet s e1 e2 -> case e1 of
    EInt i -> do
      dl2 <- exprToDeclList' e2
      return $ [DOriZ s i] ++ dl2
    EBinOp op (EVariable r1) (EVariable r2) -> do
      dl2 <- exprToDeclList' e2
      return $ [DBinOp op r1 r2 s] ++ dl2
    EApp e3 e4 -> do
      dl2 <- exprToDeclList' e2
      (f,as) <- appToFunAndArgs $ EApp e3 e4
      return $ [DCall f as s] ++ dl2
    EVariable s1 -> do
      dl2 <- exprToDeclList' e2
      return $ [DMove s s1] ++ dl2
    EFun _ _ -> left "EFun expression exists in the right side of a let."
    _ -> left "Inappropriate expression exists in the right side of a let."
  ELetRec s1 s2 e1 e2 -> do
    (as,b) <- funToArgsAndBody e1
    dlb <- exprToDeclList' b
    dl2 <- exprToDeclList' e2
    return $ [DLabel s1] ++ [DArgs (s2:as)] ++ dlb ++ dl2
  EVariable s -> return [DRet s]
  EInt _ -> left "expr is EInt."
  EBinOp _ _ _ -> left "expr is EBinOp."
  EApp _ _ -> left "expr is EApp"
  _ -> left "expr is nor if/let/let rec."

