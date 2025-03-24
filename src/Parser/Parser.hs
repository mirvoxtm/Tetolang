{-# LANGUAGE OverloadedStrings #-}
module Parser.Parser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific (toRealFloat)

import Parser.Ast

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseNumber :: Parser Expression
parseNumber = Num . toRealFloat <$> lexeme L.scientific

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseArray :: Parser Expression
parseArray = do
  _     <- symbol "["
  elems <- sepBy parseExpr (symbol ",")
  _     <- symbol "]"
  return (Arr elems)

operatorFunc :: Parser String
operatorFunc = choice $ map symbol ["|", "+", "-", "*", "/", "'","\""]

parseBuiltinFunction :: Parser Expression
parseBuiltinFunction = do
  op <- choice $ map symbol ["+", "-", "*", "/", "|"]
  numOpt <- optional (lexeme L.scientific)
  case numOpt of
    Nothing -> return (Fun op)
    Just n  -> return (Fun (op ++ show (toRealFloat n)))


parseReduce :: Parser Expression
parseReduce = do
  _ <- symbol ">" 
  f <- parseBuiltinFunction
  a <- parseExpr
  return (Reduce f a)

parseMap :: Parser Expression
parseMap = do
  _ <- symbol "<"
  f <- parseBuiltinFunction
  a <- parseExpr
  return (Map f a)

parseFunction :: Parser Expression
parseFunction = do
  op <- operatorFunc
  case op of
    "|" -> do { e <- parseExpr; return (Range e) }
    "\"" -> do { e <- parseExpr; return (Max e) }
    "'" -> do { e <- parseExpr; return (Min e) }
    "+" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Sum e1 e2) }
    "-" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Sub e1 e2) }
    "*" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Mul e1 e2) }
    "/" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Div e1 e2) }
    _   -> fail ("Unknown function operator: " ++ op)

parseExpr :: Parser Expression
parseExpr = try parseReduce
          <|> try parseMap
          <|> try parseFunction
          <|> try parseArray
          <|> try parseNumber
          <|> parens parseExpr
  

testParse :: String -> IO ()
testParse input =
  case parse (sc *> parseExpr <* eof) "" input of
    Left err -> putStr (errorBundlePretty err)
    Right expr -> print expr
