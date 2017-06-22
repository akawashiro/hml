import Parse

data VMCode = MInt Int
            | MArg Int
            | MLocal Int
            | MJump String [VMCode]
            | MLabel String
            | MBinOp BinOp VMCode VMCode
            | MRet VMCode
            deriving Show

type VMProgram = [VMCode]

programToVMProgram p = concat $ map exprToVMProgram p

exprToVMProgram exp = case exp of
  | EInt i -> 
  | EBool Bool 
  | EBinOp BinOp Expr Expr
  | EIf Expr Expr Expr
  | ELet String Expr Expr
  | EFun String Expr
  | EApp Expr Expr
  | ELetRec String String Expr Expr
  | EVariable String
