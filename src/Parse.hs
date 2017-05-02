module Parse
  ( stringToProgram,
    Program
  ) where

import Text.ParserCombinators.Parsec

type Program = [Expr]

data Expr = EInt Int 
            | EBool Bool 
            | EBinOp BinOp Expr Expr
            | EIf Expr Expr Expr
            | ELet String Expr Expr
            | EFun String Expr
            | EApp Expr Expr
            | ELetRec String Expr Expr

data BinOp = Plus | Mult

parseBinOp :: Parser BinOp
parseBinOp = (string "+" >> return Plus) <|> (string "*" >> return Mult)

parseBool :: Parser Expr
parseBool = (string "True" >> return (EBool True)) <|> (string "True" >> return (EBool True))
parseIf :: Parser Expr
parseIf = do
  string "if"
  spaces
  c <- parseExpr
  spaces
  string "then"
  spaces
  e1 <- parseExpr
  spaces
  string "else"
  spaces
  e2 <- parseExpr
  return (EIf c e1 e2)

parseValiableName :: Parser String
parseValiableName = do
  a <- lower
  as <- many alphaNum
  return (a:as)

parseLet :: Parser Expr
parseLet = do
  string "let"
  spaces
  x <- parseValiableName
  spaces
  string "="
  spaces
  e1 <- parseExpr
  string "in"
  spaces
  e2 <- parseExpr
  return (ELet x e1 e2)

parseFun :: Parser Expr
parseFun = do
  string "fun"
  spaces
  x <- parseValiableName
  spaces
  string "->"
  spaces
  e <- parseExpr
  return (EFun x e)

parseExpr :: Parser Expr
parseExpr = undefined

stringToProgram :: String -> Program
stringToProgram = undefined
