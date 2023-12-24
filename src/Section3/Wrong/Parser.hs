{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Section3.Wrong.Parser
    ( Expr (..)
    , Section3.Wrong.Parser.parse
    ) where

import Prelude hiding (succ, pred)
import Data.Functor.Identity
import Text.Parsec as P
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

data Expr
    = ETrue
    | EFalse
    | EZero
    | EIf Expr Expr Expr
    | ESucc Expr
    | EPred Expr
    | EIsZero Expr
    | EWrong  -- is used only in the evaluation
    deriving (Show, Eq)

type Parser a = ParsecT String () Identity a

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

parse :: String -> Either ParseError Expr
parse = P.parse parser ""

parser :: Parser Expr
parser = const <$> expr <*> eof

expr :: Parser Expr
expr =  parens expr
    <|> true
    <|> false
    <|> zero
    <|> succ
    <|> pred
    <|> isZero
    <|> ifThenElse

true :: Parser Expr
true = keyword "true" >> return ETrue

false :: Parser Expr
false = keyword "false" >> return EFalse

zero :: Parser Expr
zero = keyword "0" >> return EZero

succ :: Parser Expr
succ = fmap ESucc $ keyword "succ" >> expr

pred :: Parser Expr
pred = fmap EPred $ keyword "pred" >> expr

isZero :: Parser Expr
isZero = fmap EIsZero $ keyword "iszero" >> expr

ifThenElse :: Parser Expr
ifThenElse = do
    predExpr <- pIf
    thenExpr <- pThen
    elseExpr <- pElse
    return $ EIf predExpr thenExpr elseExpr
    where
        pIf = keyword "if" >> expr
        pThen = keyword "then" >> expr
        pElse = keyword "else" >> expr

keyword :: String -> Parser ()
keyword name = try $ string name >> notFollowedBy alphaNum >> whiteSpace
