module Emit where 

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Parser

exprToLLVM :: Expr -> LLVM ()
exprToLLVM = undefined
exprToLLVM
