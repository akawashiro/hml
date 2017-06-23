import Parse
import Data.Map as Map
import Control.Monad.State

-- index local variable such that MLocal 0, MLocal 1,...
-- change arguments to MArg 0, MArg 1,...
-- change EApp to MJump

data VMCode = MInt Int
            | MArg Int
            | MLocal Int
            | MCall String [VMCode] VMCode -- last operand is store destination
            | MLabel String
            | MBinOp BinOp VMCode VMCode VMCode -- last operand is store destination
            | MLoad VMCode VMCode
            | MRet VMCode
            deriving Show

type VMProgram = [VMCode]

programToVMProgram p = concat $ map exprToVMProgram p

exprToVMProgram :: Expr -> State (Map.Map String VMCode) [VMCode]
exprToVMProgram exp = case exp of
  | EInt i -> [MInt i]
  | EBinOp BinOp Expr Expr
  | EIf Expr Expr Expr
  | ELet String Expr Expr
  | EApp Expr Expr
  | ELetRec String String Expr Expr
  | EVariable String
