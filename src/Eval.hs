{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Eval where
import Parser.Ast
import Text.Read (readMaybe)
import Parser.Parser
import Returns
import Data.Fixed (mod')
import Data.List
import Data.Char
import Data.Maybe (fromMaybe)
import qualified GHC.Err as Err

eval :: Expression -> Value
eval (Num n) = Numerical n

eval (Id e) = eval e

eval (Char c) = Characterial c

eval (Boolean b) = Booleanical b

eval (Eq x y) = if eqValue (eval x) (eval y) then Booleanical True else Booleanical False

eval (Neq x y) = if eqValue (eval x) (eval y) then Booleanical False else Booleanical True

eval (If cond e1 e2) = if eval cond == Booleanical True then eval e1 else eval e2

eval (Sum e1 e2) = (eval e1) +++ (eval e2)

eval (Sub e1 e2) = (eval e1) -/- (eval e2)

eval (Mul e1 e2) = (eval e1) */* (eval e2)

eval (Div e1 e2) = (eval e1) /|\ (eval e2)

eval (Exp e1 e2) = (eval e1) ^/^ (eval e2)

eval (Nub e) = case eval e of
    Vectorial xs -> Vectorial (nub xs)
    _ -> errorWithoutStackTrace "Nub expects a vector"

eval (Len e) = case eval e of
    Vectorial xs -> Numerical (fromIntegral (length xs))
    _ -> errorWithoutStackTrace "Len expects a vector"

eval (Mod e1 e2) =
    case (eval e1, eval e2) of
        (Numerical x, Numerical y) ->
            if y == 0 then errorWithoutStackTrace "Modulo by zero"
            else Numerical (mod' x y)
        _ -> errorWithoutStackTrace "Modulo expects two numbers"

eval (Fact e)    = applyUnary (\v -> case v of
    Numerical n  -> Numerical (product [1..n])
    Vectorial xs -> Vectorial (map (\(Numerical n) -> Numerical (product [1..n])) xs)
    ) (eval e)

eval (Arr xs)    = Vectorial (map eval xs)

eval (Neg e) = case e of
    Boolean b -> Booleanical (not b)
    _ -> applyUnary negValue (eval e)

eval (Range e) =
    case eval e of
        Numerical n -> Vectorial (map (Numerical . fromIntegral) [1 .. floor n])
        Vectorial xs -> Numerical (fromIntegral (length xs))
        _ -> errorWithoutStackTrace "Range function expects a numerical expression"

eval (Max e) = case eval e of
    Vectorial xs -> Numerical (maximum (map toNum xs))
    Numerical n  -> Numerical n
    _            -> errorWithoutStackTrace "Max expects a vector or number"

eval (Min e) = case eval e of
    Vectorial xs -> Numerical (minimum (map toNum xs))
    Numerical n  -> Numerical n
    _            -> errorWithoutStackTrace "Min expects a vector or number"

eval (Reduce fExpr arrExpr) = reduceFunc fExpr arrExpr

eval (Map fExpr arrExpr)    = mapFunc fExpr arrExpr

eval (PartialEq e) =
    Function 1 (\[x] -> if eqValue x (eval e) then Booleanical True else Booleanical False)

eval (PartialNeq e) =
    Function 1 (\[x] -> if eqValue x (eval e) then Booleanical False else Booleanical True)

eval (Filter fExpr arrExpr) =
    case (eval fExpr, eval arrExpr) of
        (Function 1 f, Vectorial xs) -> Vectorial (filter (toBool . f . return) xs)
        _ -> errorWithoutStackTrace "Filter expects a function and a vector"

eval (Reverse e) =
    case eval e of
        Vectorial xs -> Vectorial (reverse xs)
        _ -> errorWithoutStackTrace "Reverse function expects a vector"

eval (Rotate e1 e2) =
    case (eval e1, eval e2) of
        (Numerical n, Vectorial xs) ->
            let len = length xs
                shift = round n `mod` len
            in Vectorial (drop shift xs ++ take shift xs)
        (Vectorial xs, Numerical n) ->
            let len = length xs
                shift = round n `mod` len
            in Vectorial (drop shift xs ++ take shift xs)
        _ -> errorWithoutStackTrace "Rotate expects a number and a vector"

eval (Index e1 e2) =
    case (eval e1, eval e2) of
        (Numerical n, Vectorial xs) ->
            if round n <= 0 || round n > length xs
                then errorWithoutStackTrace "Index out of bounds"
                else xs !! (round n - 1)
        (Vectorial xs, Numerical n) ->
            if round n <= 0 || round n > length xs
                then errorWithoutStackTrace "Index out of bounds"
                else xs !! (round n - 1)
        _ -> errorWithoutStackTrace "Index expects a number and a vector"
        
eval (Match e1 e2) =
    case (eval e1, eval e2) of
        (Numerical v1, Numerical v2) ->
            if v1 == v2 then Booleanical True else Booleanical False
        (Numerical n, Vectorial ys) ->
            case findIndex (\y -> n == toNum y) ys of
                Just idx -> Numerical ((fromIntegral idx) + 1)
                Nothing  -> Numerical 0.0
        (Vectorial xs, Numerical n) ->
            Vectorial (map (\x -> if toNum x == n then Booleanical True else Booleanical False) xs)
        (Booleanical b1, Booleanical b2) ->
            if b1 == b2 then Booleanical True else Booleanical False
        (Booleanical b, Vectorial ys) ->
            Vectorial (map (\y -> if b == toBool y then Booleanical True else Booleanical False) ys)
        (Vectorial xs, Booleanical b) ->
            Vectorial (map (\x -> if toBool x == b then Booleanical True else Booleanical False) xs)
        (Characterial c1, Characterial c2) ->
            if c1 == c2 then Booleanical True else Booleanical False
        (Characterial c, Vectorial ys) ->
            Vectorial (map (\y -> if c == toChar y then Booleanical True else Booleanical False) ys)
        (Vectorial xs, Characterial c) ->
            Vectorial (map (\x -> if toChar x == c then Booleanical True else Booleanical False) xs)
        (Vectorial xs, Vectorial ys) ->
            if length xs /= length ys
                then errorWithoutStackTrace "Match expects vectors of the same length."
                else Vectorial (zipWith (\x y -> if eqValue x y then Booleanical True else Booleanical False) xs ys)
        _ -> errorWithoutStackTrace "Invalid types for Match."

eval (Flat e) =
    case eval e of
        Vectorial xs -> Vectorial (concatMap (\v -> case v of
            Vectorial ys -> ys
            _            -> [v]
            ) xs)
        _ -> errorWithoutStackTrace "Flat expects a vector"

eval (Fun op) =
    fromMaybe (parseCustomFun op) (lookup op funTable)
  where
    funTable =
      [ ("=", Function 2 (\[x, y] -> if eqValue x y then Booleanical True else Booleanical False))
      , ("~", Function 2 (\[x, y] -> if eqValue x y then Booleanical False else Booleanical True))
      , ("+", Function 2 (\[x, y] -> Numerical (toNum x + toNum y)))
      , ("-", Function 2 (\[x, y] -> Numerical (toNum x - toNum y)))
      , ("*", Function 2 (\[x, y] -> Numerical (toNum x * toNum y)))
      , ("/", Function 2 (\[x, y] -> Numerical (safeDiv (toNum x) (toNum y))))
      , ("^", Function 2 (\[x, y] -> Numerical (toNum x ** toNum y)))
      , ("|", Function 1 (\[x] -> rangeFunc (toNum x)))
      , ("'", Function 1 (\[x] -> applyUnary (Numerical . minimum . extractNums) x))
      , ("\"", Function 1 (\[x] -> applyUnary (Numerical . maximum . extractNums) x))
      , ("°", Function 1 (\[x] -> applyUnary (Numerical . sum . extractNums) x))
      , ("!", Function 1 (\[x] -> applyUnary (\v -> case v of
            Numerical n  -> Numerical (product [1..n])
            Vectorial xs -> Vectorial (map (\(Numerical n) -> Numerical (product [1..n])) xs)
            _            -> errorWithoutStackTrace "Factorial expects a numerical or vectorial value."
        ) x))
      , ("¬", Function 1 (\[x] -> applyUnary negValue x))
      , ("§", Function 1 (\[x] -> applyUnary (\v -> case v of
              Vectorial xs -> Vectorial (reverse xs)
              _            -> errorWithoutStackTrace "Reverse function expects a vectorial value."
        ) x))
      ]

toBool :: Value -> Bool
toBool (Booleanical b) = b
toBool _               = errorWithoutStackTrace "Expected a vector of boolean values."

toChar :: Value -> Char
toChar (Characterial c) = c
toChar _                = errorWithoutStackTrace "Expected a character value."

parseCustomFun :: String -> Value
parseCustomFun op
    | head op `elem` "+-*/=~" && length op > 1 =
        let rest = tail op
        in case head op of
              '+' -> case readMaybe rest :: Maybe Double of
                         Just n  -> Function 1 (\[x] -> Numerical (toNum x + n))
                         Nothing -> errorWithoutStackTrace ("Invalid numeric constant: " ++ rest)
              '-' -> case readMaybe rest :: Maybe Double of
                         Just n  -> Function 1 (\[x] -> Numerical (toNum x - n))
                         Nothing -> errorWithoutStackTrace ("Invalid numeric constant: " ++ rest)
              '*' -> case readMaybe rest :: Maybe Double of
                         Just n  -> Function 1 (\[x] -> Numerical (toNum x * n))
                         Nothing -> errorWithoutStackTrace ("Invalid numeric constant: " ++ rest)
              '/' -> case readMaybe rest :: Maybe Double of
                         Just n  -> Function 1 (\[x] -> Numerical (toNum x / n))
                         Nothing -> errorWithoutStackTrace ("Invalid numeric constant: " ++ rest)
              '=' -> case readMaybe rest :: Maybe Double of
                         Just n  -> Function 1 (\[x] -> if toNum x == n then Booleanical True else Booleanical False)
                         Nothing -> Function 1 (\[x] ->
                                    if eqValue x (stringVal rest)
                                      then Booleanical True
                                      else Booleanical False)
              '~' -> case readMaybe rest :: Maybe Double of
                         Just n  -> Function 1 (\[x] -> if toNum x == n then Booleanical False else Booleanical True)
                         Nothing -> Function 1 (\[x] ->
                                    if eqValue x (stringVal rest)
                                      then Booleanical False
                                      else Booleanical True)
              _   -> errorWithoutStackTrace ("Invalid operator: " ++ op)
    | otherwise = errorWithoutStackTrace ("Unknown function operator: " ++ op)

stringVal :: String -> Value
stringVal s = Vectorial (map Characterial s)

eqValue :: Value -> Value -> Bool
eqValue (Numerical a) (Numerical b)     = a == b
eqValue (Vectorial xs) (Vectorial ys) 
  | length xs == length ys              = and (zipWith eqValue xs ys)
  | otherwise                           = False
eqValue (Booleanical a) (Booleanical b) = a == b
eqValue (Characterial a) (Characterial b) = a == b
eqValue _ _                             = False

extractNums :: Value -> [Double]
extractNums (Numerical n)  = [n]  -- Wrap single numbers in a list
extractNums (Vectorial xs) = concatMap extractNums xs
extractNums _              = errorWithoutStackTrace "Expected a numerical vector"

reduceFunc :: Expression -> Expression -> Value
reduceFunc fExpr arrExpr =
    case eval fExpr of
        Function 2 f -> case eval arrExpr of
            Vectorial xs -> Numerical (foldl (\acc x -> toNum (f [Numerical acc, x])) 0 xs)
            _ -> errorWithoutStackTrace "Reduce expects a vector as its second argument"
        _ -> errorWithoutStackTrace "Reduce expects a binary function"

mapFunc :: Expression -> Expression -> Value
mapFunc fExpr arrExpr = case eval fExpr of
    Function 1 f -> case eval arrExpr of
        Vectorial xs -> Vectorial (map (f . return) xs)
        _ -> errorWithoutStackTrace "Map expects a vector as its second argument"
    Function 2 f -> case eval arrExpr of
        Vectorial xs -> Vectorial (map (\x -> f [x]) xs)
        _ -> errorWithoutStackTrace "Map expects a vector as its second argument"
    _ -> errorWithoutStackTrace "Map expects a function as its first argument"

negValue :: Value -> Value
negValue (Numerical n) = Numerical (-n)
negValue (Vectorial xs) = Vectorial (map negValue xs)
negValue (Booleanical b) = Booleanical (not b)

safeDiv :: Double -> Double -> Double
safeDiv x y
    | y == 0 = errorWithoutStackTrace "Division by zero"
    | otherwise = x / y

rangeFunc :: Double -> Value
rangeFunc x = Vectorial (map (Numerical . fromIntegral) [1 .. floor x])

applyUnary :: (Value -> Value) -> Value -> Value
applyUnary f (Booleanical b) = f (Booleanical b)
applyUnary f (Vectorial xs) = Vectorial (map f xs)
applyUnary f v              = f v