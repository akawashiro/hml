{-# LANGUAGE FlexibleContexts #-}

module Parse where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Combinator as C (chainl1)

data Expr = EInt Int 
            | EBool Bool 
            | EBinOp BinOp Expr Expr
            | EIf Expr Expr Expr
            | ELet String Expr Expr
            | EFun String Expr
            | EApp Expr Expr
            | ELetRec String String Expr Expr
            | EVariable String

data BinOp = Plus | Mult | Lt

instance Show BinOp where
  show Plus = "+"
  show Mult = "*"
  show Lt   = "<"

instance Show Expr where
  show (EInt i) = show i
  show (EBool b) = show b
  show (EBinOp o e1 e2) = show e1 ++ " " ++ show o ++ " " ++ show e2
  show (EVariable s) = s
  show (EFun s e) = "fun " ++ s ++ " -> " ++ show e
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (ELet s e1 e2) = "let " ++ s ++ " = " ++ show e1 ++ " in\n " ++ show e2
  show (ELetRec s1 s2 e1 e2) = "let rec " ++ s1 ++ " = fun " ++ s2 ++ " -> " ++ show e1 ++ " in\n " ++ show e2
  show (EApp e1 e2) = show e1 ++ " " ++ show e2


parseTest f = parse f "Test"

stringToExpr :: String -> Either ParseError Expr
stringToExpr = parse parseSentence "Parse.hs"

parseSentence :: Parser Expr
parseSentence = do
  e<-parseExpr
  spaces
  string ";;"
  spaces
  return e

parseExpr = do
  spaces
  e <- parseExpr'
  spaces
  return e

parseExpr' :: Parser Expr
parseExpr' = try parseBool <|>
             try parseIf <|>
             try parseLet <|>
             try parseLetRec <|> 
             try parseFun <|>
             try parseBinOp <|>
             try parseVariable

reservedName = ["let","in","fun","if","then","else"]

parseVariable :: Parser Expr
parseVariable = try (do
  spaces 
  s <- parseVariableName 
  spaces
  return (EVariable s))

parseBinOp :: Parser Expr
parseBinOp = try parseLtExpr

parseLtExpr :: Parser Expr
parseLtExpr = try parseLtExpr' <|> try parsePlusExpr

parseLtExpr' = do
  e1 <- parsePlusExpr
  spaces
  char '<'
  spaces
  e2 <- parsePlusExpr
  return (EBinOp Lt e1 e2)

parsePlus = parse parsePlusExpr "Plus"

parsePlusExpr = try parsePlusExpr' <|> try parseMultExpr
parsePlusExpr' = do
  e1 <- parseMultExpr
  spaces
  char '+'
  spaces
  e2 <- parsePlusExpr
  return (EBinOp Plus e1 e2) 

parseMult = parse parseMultExpr "Mult"
parseMult' = parse parseMultExpr' "Mult"

parseMultExpr = try parseMultExpr' <|> try parseAppExpr
parseMultExpr' = do
  e1 <- parseAppExpr 
  spaces
  char '*'
  spaces
  e2 <- parseMultExpr
  return (EBinOp Mult e1 e2)

parseApp = parse parseAppExpr "App"

parseAppExpr = parseAppExpr' <|> parseAExpr
parseAppExpr' = try (C.chainl1 parseAExpr parseAppExpr'')
parseAppExpr'' :: Parser (Expr -> Expr -> Expr)
parseAppExpr'' = do
  spaces;
  return EApp

parseA = parse parseAExpr "A"
parseAExpr = try parseVariable <|>
             try parseInt <|>
             try parseBool <|>
             try (do { char '('; spaces; e<-parseExpr; spaces; char ')'; spaces; return e}) <|>
             parseVariable

parseBool :: Parser Expr
parseBool = (string "True" >> return (EBool True)) <|> (string "True" >> return (EBool True))

parseInt :: Parser Expr
parseInt = try (do { n<-many1 digit; return (EInt (read n::Int))})

parseIf :: Parser Expr
parseIf = do {string "if"; spaces; c <- parseExpr; spaces; string "then"; spaces; e1 <- parseExpr; 
             spaces; string "else"; spaces; e2 <- parseExpr; return (EIf c e1 e2)}

parseVariableName :: Parser String
parseVariableName = try (do
  spaces
  a <- lower
  as <- many alphaNum
  spaces
  if (a:as) `elem` reservedName 
  then fail "reservedName is used for variable name."
  else return (a:as))

parseLet :: Parser Expr
parseLet = do
  string "let"
  spaces
  x <- parseVariableName
  spaces
  string "="
  spaces
  e1 <- parseExpr
  spaces
  string "in"
  spaces
  e2 <- parseExpr
  spaces
  return (ELet x e1 e2)

parseLetRec :: Parser Expr
parseLetRec = do
  string "let"
  spaces
  string "rec"
  spaces
  x <- parseVariableName
  spaces
  string "="
  spaces
  string "fun"
  spaces
  y <- parseVariableName
  spaces
  string "->"
  spaces
  e1 <- parseExpr
  spaces
  string "in"
  spaces
  e2 <- parseExpr
  spaces
  return (ELetRec x y e1 e2)

parseFun :: Parser Expr
parseFun = do
  string "fun"
  spaces
  x <- parseVariableName
  spaces
  string "->"
  spaces
  e <- parseExpr
  return (EFun x e)
