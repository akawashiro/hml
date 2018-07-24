{-# LANGUAGE FlexibleContexts #-}

-- module Parse (stringToExp, Exp(..)) where
module Parse where

import           Control.Monad.Identity
import           Data.Either
import           Data.Maybe
import           Debug.Trace                            (trace)
import qualified Text.Parsec.Combinator                 as C (chainl1, chainr1)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

stringToExp :: String -> Either String Exp
stringToExp input = either (\x->Left $show x) (\x->Right x) (parse parseExp "Parse.hs" input)

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

natDef :: P.GenLanguageDef String () Identity
natDef = emptyDef { P.reservedNames = keywords, P.reservedOpNames = operators }

keywords :: [String]
keywords = [ "let", "rec", "in", "true", "false", "evalto", "if", "then", "else", "fun"]

operators = [ "=", "->", "+", "-", "*", "<"]


kwLet         = P.reserved lexer "let"
kwRec         = P.reserved lexer "rec"
kwIn          = P.reserved lexer "in"
kwTrue        = P.reserved lexer "true"
kwFalse       = P.reserved lexer "false"
kwIf          = P.reserved lexer "if"
kwThen        = P.reserved lexer "then"
kwElse        = P.reserved lexer "else"
kwFun         = P.reserved lexer "fun"
kwEqual       = P.reservedOp lexer "="
kwArrowSymbol = P.reservedOp lexer "->"
kwPlusSymbol  = P.reservedOp lexer "+"
kwMinusSymbol = P.reservedOp lexer "-"
kwTimesSymbol = P.reservedOp lexer "*"
kwLessSymbol  = P.reservedOp lexer "<"

lexer = P.makeTokenParser natDef
parens = P.parens lexer
whiteSpace = P.whiteSpace lexer

parseExp :: Parser Exp
parseExp = parseExpIf <|>
           try parseExpLet <|>
           parseExpRec <|>
           parseExpLt

parseExpIf :: Parser Exp
parseExpIf = do kwIf; e1<-parseExp; kwThen; e2<-parseExp; kwElse; EIf e1 e2 <$> parseExp;

parseExpLet :: Parser Exp
parseExpLet = do kwLet; x<-parseVar; kwEqual; e1<-parseExp; kwIn; ELet x e1 <$> parseExp;

parseExpRec :: Parser Exp
parseExpRec = do kwLet; kwRec; x<-parseVar; ys <- many1 parseVar; kwEqual; e1<-parseExp; kwIn; ERec x ys e1 <$> parseExp;

parseExpLt :: Parser Exp
parseExpLt =  do 
  e1<-parseExpP
  (kwLessSymbol >> EOp OLess e1 <$> parseExpP) <|> (return e1)

parseExpP :: Parser Exp
parseExpP = C.chainl1 parseExpM (kwPlusSymbol >> return (EOp OPlus))

parseExpM :: Parser Exp
parseExpM = C.chainl1 parseExpT (kwMinusSymbol >> return (EOp OMinus))

parseExpT :: Parser Exp
parseExpT = C.chainl1 parseExpApp (kwTimesSymbol >> return (EOp OTimes))

parseExpApp :: Parser Exp
parseExpApp = do
  es <- many1 parseExpAtom
  if length es == 1 then return (head es) else return (EApp (head es) (tail es))

parseExpAtom :: Parser Exp
parseExpAtom = parens parseExp
               <|> (EBool <$> parseBool)
               <|> try (EInt <$> parseInt)
               <|> (EVar <$> parseVar)

parseBool :: Parser Bool
parseBool = (kwTrue >> return True) <|> (kwFalse >> return False)


parseInt :: Parser Integer
parseInt = (do whiteSpace; char '-'; ds<-many1 digit; whiteSpace; return $ -1 * read ds)
           <|> (do whiteSpace; ds<-many1 digit; whiteSpace; return $ read ds)


parseVar :: Parser Var
parseVar = Var <$> P.identifier lexer

