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

eval :: Expression -> Value
eval (Num n) = Numerical n
eval (Sum e1 e2) = (eval e1) +++ (eval e2)
eval (Sub e1 e2) = (eval e1) -/- (eval e2)
eval (Mul e1 e2) = (eval e1) */* (eval e2)
eval (Div e1 e2) = (eval e1) /|\ (eval e2)
eval (Arr xs) = Vectorial (map eval xs)
eval (Fun op) = case op of
    "+" -> Function 2 (\[x, y] -> x + y)
    "-" -> Function 2 (\[x, y] -> x - y)
    "*" -> Function 2 (\[x, y] -> x * y)
    "/" -> Function 2 (\[x, y] -> safeDiv x y)
    _   -> error ("Unknown function operator: " ++ op)
eval (Range e) =
    case eval e of
        Numerical n -> Vectorial (map (Numerical . fromIntegral) [1 .. floor n])
        _ -> error "Range function expects a numerical expression"
eval (Max e) = 
    case eval e of
        Numerical n -> Numerical n
        Vectorial xs -> Numerical (maximum (map toNum xs))

eval (Reduce fExpr arrExpr) =
    case eval fExpr of
        Function arity f ->
            if arity == 2 then
                case eval arrExpr of
                    Vectorial xs -> Numerical (foldl (\acc x -> f [acc, x]) 0 (map toNum xs))
                    _ -> error "Reduce expects a vector as its second argument"
            else
                error "Reduce expects a binary function"
        _ -> error "Reduce expects a function as its first argument"

main :: IO ()
main = do
    forever $ do
        putStr "teto> "
        input <- getLine
        case parse (sc *> parseExpr <* eof) "" input of
            Left err -> putStrLn $ errorBundlePretty err
            Right expr -> print (eval expr)