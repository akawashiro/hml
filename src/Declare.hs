module Declare where

import Parse
import Control.Monad.State
import Control.Monad.Trans.Either

data Instruction = IInt Int
            | IAdd  String String String
            | IMul  String String String
            | ISlt  String String String
            | IBeqz String String -- First operand is condition
            | IOriZ String Int
            | ICall String [String] String
            | IArgs [String]
            | IMove String String
            | IRet String
            | ILabel String
            | IJump String
            | IJal String
            | IJR   String
            | ISw String String Int -- sw r1 r2 offset offset(r2) = r1
            | ILw String String Int -- lw r1 r2 offset r1 = offset(r2)
            | IStore
            | ISysStore
            | IRestore
            | ISysRestore
            | ISys

type Declare = [Instruction]

instance Show Instruction where
  show (IInt i)             = "\t" ++ show i ++ "\n"
  show (IAdd rd rs rt)      = "\t" ++ "addu " ++ rd ++ "," ++ rs ++ "," ++ rt ++ "\n"
  show (IMul rd rs rt)      = "\t" ++ "mul " ++ rd ++ "," ++ rs ++ "," ++ rt ++ "\n"
  show (ISlt rd rs rt)      = "\t" ++ "slt " ++ rd ++ "," ++ rs ++ "," ++ rt ++ "\n"
  show (IBeqz c l)          = "\t" ++ "beqz " ++ c ++ "," ++ l ++ "\n"
  -- show (IOriZ r i)          = if 0<= i then "\t" ++ "ori " ++ r ++ ",$0," ++ show i ++ "\n"
                                       -- else "\t" ++ "ori " ++ r ++ ",$0," ++ show (65536+i) ++ "\n"
  show (IOriZ r i)          = "\tli " ++ r ++ "," ++ show i ++ "\n"
  show (ICall f as rd)      = "\t" ++ rd ++ " = " ++ f ++ " " ++ show as ++ "\n"
  show (IArgs as)           = "\t" ++ "args " ++ show as ++ "\n"
  show (IMove rs rt)        = "\t" ++ "move " ++ rs ++ "," ++ rt ++ "\n"
  show (IRet rs)            = "\t" ++ "ret " ++ rs ++ "\n"
  show (ILabel l)           = l ++ ":" ++ "\n"
  show (IJump l)            = "\t" ++ "j " ++ l ++ "\n"
  show (IJal l)             = "\t" ++ "jal " ++ l ++ "\n"
  show (IJR r)              = "\t" ++ "jr " ++ r ++ "\n"
  show IStore               = "\t" ++ "store" ++ "\n"
  show IRestore             = "\t" ++ "restore" ++ "\n"
  show ISysStore               = "\t" ++ "sysstore" ++ "\n"
  show ISysRestore             = "\t" ++ "sysrestore" ++ "\n"
  show (ISw rs rt n)        = "\t" ++ "sw " ++ rs ++ "," ++ show n ++ "(" ++ rt ++ ")\n"
  show (ILw rs rt n)        = "\t" ++ "lw " ++ rs ++ "," ++ show n ++ "(" ++ rt ++ ")\n"
  show ISys                 = "\t" ++ "syscall" ++ "\n"


exprToDeclareList :: Expr -> Either String [Declare]
exprToDeclareList exp = case exp of
  ELetRec s1 s2 e1 e2 -> do
    (as,b) <- funToArgsAndBody e1
    dlb <- exprToInstructionList b
    dl2 <- exprToDeclareList e2
    return (([ILabel s1] ++ [IArgs (s2:as)] ++ dlb) : dl2)
  _ -> do
    dl <- exprToInstructionList exp
    return [dl]

exprToInstructionList :: Expr -> Either String [Instruction]
exprToInstructionList exp = evalState (runEitherT (exprToInstructionList' exp)) 0

makeIfLabel :: EitherT String (State Int) String
makeIfLabel = do
  i <- lift get
  lift (put (i+1))
  return $ "_if_label_" ++ show i

appToFunAndArgs :: Expr -> EitherT String (State Int) (String,[String])
appToFunAndArgs (EVariable s) = return (s,[])
appToFunAndArgs (EApp e (EVariable s)) = do
  (f,as) <- appToFunAndArgs e
  return (f,s:as)
appToFunAndArgs _ = left "Application expression is not inappropriate."

-- funToArgsAndBody :: Expr -> EitherT String (State Int) ([String],Expr)
funToArgsAndBody (EFun s e) = do
  (as,b) <- funToArgsAndBody e
  return (s:as,b)
funToArgsAndBody e = return ([],e)

exprToInstructionList' :: Expr -> EitherT String (State Int) [Instruction]
exprToInstructionList' exp = case exp of
  EIf e1 e2 e3 -> case e1 of
    EVariable s -> do
      l1 <- makeIfLabel
      l2 <- makeIfLabel
      dl2 <- exprToInstructionList' e2
      dl3 <- exprToInstructionList' e3
      return $ [IBeqz s l1] ++ [IJump l2,ILabel l1] ++ dl3 ++ [ILabel l2] ++ dl2
    EInt i -> do
      dl1 <- exprToInstructionList' e1
      dl2 <- exprToInstructionList' e2
      return $ if i /= 0 then dl1 else dl2
    _ -> left "If condition is not variable or constant."
  ELet s e1 e2 -> case e1 of
    EInt i -> do
      dl2 <- exprToInstructionList' e2
      return $ IOriZ s i : dl2
    EBinOp Plus (EVariable r1) (EVariable r2) -> do
      dl2 <- exprToInstructionList' e2
      return $ IAdd s r1 r2 : dl2
    EBinOp Mult (EVariable r1) (EVariable r2) -> do
      dl2 <- exprToInstructionList' e2
      return $ IMul s r1 r2 : dl2
    EBinOp Lt (EVariable r1) (EVariable r2) -> do
      dl2 <- exprToInstructionList' e2
      return $ ISlt s r1 r2 : dl2
    EApp e3 e4 -> do
      dl2 <- exprToInstructionList' e2
      (f,as) <- appToFunAndArgs $ EApp e3 e4
      return $ ICall f as s : dl2
    EVariable s1 -> do
      dl2 <- exprToInstructionList' e2
      return $ IMove s s1 : dl2
    EFun _ _ -> left "EFun expression exists in the right side of a let."
    _ -> left "Inappropriate expression exists in the right side of a let."
  ELetRec s1 s2 e1 e2 -> do
    (as,b) <- funToArgsAndBody e1
    dlb <- exprToInstructionList' b
    dl2 <- exprToInstructionList' e2
    return $ [ILabel s1] ++ [IArgs (s2:as)] ++ dlb ++ dl2
  EVariable s -> return [IRet s]
  EInt _ -> left "expr is EInt."
  EBinOp{} -> left "expr is EBinOp."
  EApp _ _ -> left "expr is EApp"
  _ -> left "expr is nor if/let/let rec."

