{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
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

parseString :: Parser Expression
parseString = do
  _   <- symbol "\""
  str <- manyTill L.charLiteral (symbol "\"")
  case str of
    [c] -> return (Char c)
    cs  -> return (Arr (map Char cs))

{-
  ?
-}

operatorFunc :: Parser String
operatorFunc = choice $ map symbol ["=", "~", "|", "+", "-", "*", "^", "!", "/", "_","¢","¬", "°", "§", "@", "#", "%", "&", "$", "£", "\'"]

parseBuiltinFunction :: Parser Expression
parseBuiltinFunction = do
  op <- choice $ map symbol ["=", "~", "+", "-", "*", "^", "!", "/", "|", "_","¢", "¬", "°", "§", "$", "£", "\'"]
  numOpt <- optional (lexeme L.scientific)
  case numOpt of
    Nothing -> return (Fun op)
    Just n  -> return (Fun (op ++ show (toRealFloat n)))

parseIfThenElse :: Parser Expression
parseIfThenElse = do
  _ <- string "{"
  cond <- parseExpr
  _ <- string "\\"
  sc
  e1 <- parseExpr
  _ <- string "}"
  sc
  e2 <- parseExpr
  return (If cond e1 e2)

parseBool :: Parser Expression
parseBool = do
  b <- lexeme (string "O" <|> string "X")
  if b == "O" then return (Boolean True) else return (Boolean False)

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

parsePartialEquality :: Parser Expression
parsePartialEquality = do
  _ <- symbol "="
  e <- parseExpr
  lookAhead (symbol ")")
  return (PartialEq e)

parsePartialInequality :: Parser Expression
parsePartialInequality = do
  _ <- symbol "~"
  e <- parseExpr
  lookAhead (symbol ")")
  return (PartialNeq e)

parsePredicate :: Parser Expression
parsePredicate = try parsePartialEquality <|> try parsePartialInequality <|> parseExpr

parseFilter :: Parser Expression
parseFilter = do
  _       <- symbol ":"
  fExpr   <- between (symbol "(") (symbol ")") parsePredicate
  sc
  arrExpr <- parseExpr
  return (Filter fExpr arrExpr)

parseFunction :: Parser Expression
parseFunction = do
  op <- operatorFunc
  case op of
    "=" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Eq e1 e2) }
    "~" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Neq e1 e2) }
    "|" -> do { e <- parseExpr; return (Range e) }
    "¢" -> do { e <- parseExpr; return (Max e) }
    "_" -> do { e <- parseExpr; return (Min e) }
    "+" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Sum e1 e2) }
    "-" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Sub e1 e2) }
    "*" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Mul e1 e2) }
    "/" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Div e1 e2) }
    "%" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Mod e1 e2) }
    "!" -> do { e <- parseExpr; return (Fact e) }
    "¬" -> do { e <- (try parseNegative <|> parseExpr); return (Neg e) }
    "^" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Exp e1 e2) }
    "°" -> do { e <- parseExpr; return (Id e) }
    "§" -> do { e <- parseExpr; return (Reverse e) }
    "@" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Rotate e1 e2) }
    "#" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Index e1 e2) }
    "&" -> do { e1 <- parseExpr; e2 <- parseExpr; return (Match e1 e2) }
    "$" -> do { e <- parseExpr; return (Nub e) }
    "£" -> do { e <- parseExpr; return (Len e) }
    "\'"-> do { e <- parseExpr; return (Flat e) }
    _   -> fail ("Unknown function operator: " ++ op)

parseNegative :: Parser Expression
parseNegative = do
  _ <- char '-' 
  Num n <- parseExpr
  return (Num (-n))

parseExpr :: Parser Expression
parseExpr = parseString
          <|> try parseReduce
          <|> try parseMap
          <|> try parseFilter
          <|> try parseIfThenElse
          <|> try parseBool
          <|> try parseFunction
          <|> try parseArray
          <|> try parseNumber
          <|> parens parseExpr

testParse :: String -> IO ()
testParse input =
  case parse (sc *> parseExpr <* eof) "" input of
    Left err -> putStr (errorBundlePretty err)
    Right expr -> print expr
