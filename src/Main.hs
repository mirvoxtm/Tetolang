module Main where

import Parser.Parser
import Parser.Ast
import Control.Monad
import Returns
import Text.Megaparsec (parse, eof, errorBundlePretty)

safeDiv :: Double -> Double -> Double
safeDiv x y
    | y == 0 = error "Division by zero"
    | otherwise = x / y

rangeFunc :: Double -> Value
rangeFunc x = Vectorial (map (Numerical . fromIntegral) [1 .. floor x])

applyUnary :: ([Value] -> Value) -> Value -> Value
applyUnary f (Vectorial xs) = Vectorial (map (applyUnary f) xs)
applyUnary f v              = f [v]

eval :: Expression -> Value
eval (Num n) = Numerical n
eval (Sum e1 e2) = (eval e1) +++ (eval e2)
eval (Sub e1 e2) = (eval e1) -/- (eval e2)
eval (Mul e1 e2) = (eval e1) */* (eval e2)
eval (Div e1 e2) = (eval e1) /|\ (eval e2)
eval (Arr xs) = Vectorial (map eval xs)
eval (Fun op) = case op of
    "+" -> Function 2 (\[x, y] -> Numerical (toNum x + toNum y))
    "-" -> Function 2 (\[x, y] -> Numerical (toNum x - toNum y))
    "*" -> Function 2 (\[x, y] -> Numerical (toNum x * toNum y))
    "/" -> Function 2 (\[x, y] -> Numerical (safeDiv (toNum x) (toNum y)))
    "|" -> Function 1 (\[x]    -> rangeFunc (toNum x))
    _   -> if head op == '+' && length op > 1 then
              let n = read (tail op) :: Double
              in Function 1 (\[x] -> Numerical (toNum x + n))
           else if head op == '-' && length op > 1 then
              let n = read (tail op) :: Double
              in Function 1 (\[x] -> Numerical (toNum x - n))
           else if head op == '*' && length op > 1 then
              let n = read (tail op) :: Double
              in Function 1 (\[x] -> Numerical (toNum x * n))
           else if head op == '/' && length op > 1 then
              let n = read (tail op) :: Double
              in Function 1 (\[x] -> Numerical (toNum x / n))
           else error ("Unknown function operator: " ++ op)

eval (Range e) =
    case eval e of
        Numerical n -> Vectorial (map (Numerical . fromIntegral) [1 .. floor n])
        _ -> error "Range function expects a numerical expression"

eval (Max e) = 
    case eval e of
        Numerical n -> Numerical n
        Vectorial xs -> Numerical (maximum (map toNum xs))

eval (Min e) = 
    case eval e of
        Numerical n -> Numerical n
        Vectorial xs -> Numerical (minimum (map toNum xs))

eval (Reduce fExpr arrExpr) =
    case eval fExpr of
        Function arity f ->
            if arity == 2 then
                case eval arrExpr of
                Vectorial xs ->
                    Numerical (foldl (\acc x -> let r = f [Numerical acc, x]
                                                in toNum r) 0 xs)
                _ -> error "Reduce expects a vector as its second argument"
            else
                error "Reduce expects a binary function"
        _ -> error "Reduce expects a function as its first argument"

eval (Map fExpr arrExpr) =
    case eval fExpr of
        Function a f ->
            if a == 1 then
                case eval arrExpr of
                Vectorial xs -> Vectorial (map (applyUnary f) xs)
                _ -> error "Map expects a vector as its second argument"
            else
                error "Map expects a unary function"
        _ -> error "Map expects a function as its first argument"

main :: IO ()
main = do
    forever $ do
        putStr "teto> "
        input <- getLine
        case parse (sc *> parseExpr <* eof) "" input of
            Left err -> putStrLn $ errorBundlePretty err
            Right expr -> print (eval expr)