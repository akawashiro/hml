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
            deriving Show

data BinOp = Plus | Mult | Lt deriving Show

stringToProgram :: String -> Either ParseError Program
stringToProgram = parse parseProgram "Parse.hs"

parseProgram :: Parser Program
parseProgram = many1 parseSentence

parseSentence :: Parser Expr
parseSentence = do{ e<-parseExpr; spaces; string ";;"; spaces; return e}

parseExpr :: Parser Expr
parseExpr = parseExpr' <|> parseExpr''

parseExpr' = do
  spaces
  char '('
  spaces
  e <- parseExpr'''
  spaces
  char ')'
  spaces
  return e

parseExpr'' = do
  spaces
  e <- parseExpr'''
  spaces
  return e

parseExpr''' :: Parser Expr
parseExpr''' = parseBool <|>
             parseBinOp <|>
             parseIf <|>
             parseLet <|>
             parseLetRec <|> 
             parseFun <|>
             parseAppExpr

parseBinOp :: Parser Expr
parseBinOp = parseLtExpr

parseLtExpr :: Parser Expr
parseLtExpr = parseLtExpr' <|> parsePlusExpr

parseLtExpr' = do
  e1 <- parsePlusExpr
  spaces
  char '<'
  spaces
  e2 <- parsePlusExpr
  return (EBinOp Lt e1 e2)

parsePlusExpr = parsePlusExpr' <|> parseMultExpr
parsePlusExpr' = do
  e1 <- parsePlusExpr
  spaces
  char '+'
  spaces
  e2 <- parseMultExpr
  return (EBinOp Plus e1 e2) 

parseMultExpr = parseMultExpr' <|> parseAppExpr
parseMultExpr' = do
  e1 <- parseMultExpr
  spaces
  char '*'
  spaces
  e2 <- parseAppExpr 
  return (EBinOp Mult e1 e2)

parseAppExpr = do { e1<-parseAppExpr; e2<-parseAExpr; return (EApp e1 e2) } <|> parseAExpr

parseAExpr = parseInt <|>
             parseBool <|>
             do { char '('; spaces; e<-parseExpr; spaces; char ')'; return e}

parseBool :: Parser Expr
parseBool = (string "True" >> return (EBool True)) <|> (string "True" >> return (EBool True))

parseInt :: Parser Expr
parseInt = do { n<-many1 digit; return (EInt ((read n)::Int))}

parseIf :: Parser Expr
parseIf = do {string "if"; spaces; c <- parseExpr; spaces; string "then"; spaces; e1 <- parseExpr; 
             spaces; string "else"; spaces; e2 <- parseExpr; return (EIf c e1 e2)}

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

parseLetRec :: Parser Expr
parseLetRec = do
  string "let"
  spaces
  string "rec"
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
