module Eval where
import Parser.Ast
import Parser.Parser
import Returns
import Data.Fixed (mod')
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

eval :: Expression -> Value
eval (Num n) = Numerical n
eval (Id e) = eval e
eval (Sum e1 e2) = (eval e1) +++ (eval e2)
eval (Sub e1 e2) = (eval e1) -/- (eval e2)
eval (Mul e1 e2) = (eval e1) */* (eval e2)
eval (Div e1 e2) = (eval e1) /|\ (eval e2)
eval (Exp e1 e2) = (eval e1) ^/^ (eval e2)
eval (Mod e1 e2) =
    case (eval e1, eval e2) of
        (Numerical x, Numerical y) ->
            if y == 0 then error "Modulo by zero"
            else Numerical (mod' x y)
        _ -> error "Modulo expects two numbers"
eval (Fact e)    = applyUnary (\v -> case v of
    Numerical n  -> Numerical (product [1..n])
    Vectorial xs -> Vectorial (map (\(Numerical n) -> Numerical (product [1..n])) xs)
    ) (eval e)
eval (Arr xs)    = Vectorial (map eval xs)
eval (Neg e) = applyUnary negValue (eval e)
eval (Range e) =
    case eval e of
        Numerical n -> Vectorial (map (Numerical . fromIntegral) [1 .. floor n])
        Vectorial xs -> Numerical (fromIntegral (length xs))
        _ -> error "Range function expects a numerical expression"
eval (Max e) = case eval e of
    Vectorial xs -> Numerical (maximum (map toNum xs))
    Numerical n  -> Numerical n
    _            -> error "Max expects a vector or number"

eval (Min e) = case eval e of
    Vectorial xs -> Numerical (minimum (map toNum xs))
    Numerical n  -> Numerical n
    _            -> error "Min expects a vector or number"

eval (Reduce fExpr arrExpr) = reduceFunc fExpr arrExpr
eval (Map fExpr arrExpr)    = mapFunc fExpr arrExpr
eval (Reverse e) =
    case eval e of
        Vectorial xs -> Vectorial (reverse xs)
        _ -> error "Reverse function expects a vector"
eval (Rotate e1 e2) =
    case (eval e1, eval e2) of
        (Numerical n, Vectorial xs) ->
            let len = length xs
                shift = round n `mod` len  -- Avoid going out of bounds
            in Vectorial (drop shift xs ++ take shift xs)
        _ -> error "Rotate expects a number and a vector"
eval (Index e1 e2) =
    case (eval e1, eval e2) of
        (Numerical n, Vectorial xs) ->
            if round n <= 0 || round n > length xs
                then error "Index out of bounds"
                else xs !! (round n - 1)
        _ -> error "Index expects a number and a vector"
eval (Match e1 e2) =
    case (eval e1, eval e2) of
        (Numerical v1, Numerical v2) ->
            if v1 == v2 then Numerical 1 else Numerical 0
        (Numerical n, Vectorial ys) ->
            case findIndex (\y -> n == toNum y) ys of
                Just idx -> Numerical ((fromIntegral idx) + 1)
                Nothing  -> error "Element not found in vector in Match"
        (Vectorial xs, Numerical n) ->
            Vectorial (map (\x -> if toNum x == n then Numerical 1 else Numerical 0) xs)
        (Vectorial xs, Vectorial ys) ->
            if length xs /= length ys
                then error "Match expects vectors of the same length"
                else Vectorial (zipWith (\x y -> if eqValue x y then Numerical 1 else Numerical 0) xs ys)
        _ -> error "Invalid types for Match"

eval (Fun op) =
    fromMaybe (parseCustomFun op) (lookup op funTable)
  where
    funTable =
      [ ("+", Function 2 (\[x, y] -> Numerical (toNum x + toNum y)))
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
        ) x))
      , ("¬", Function 1 (\[x] -> applyUnary negValue x))
      , ("§", Function 1 (\[x] -> applyUnary (\v -> case v of
              Vectorial xs -> Vectorial (reverse xs)
              _ -> error "Reverse function expects a vector"
        ) x))
      ]


parseCustomFun :: String -> Value
parseCustomFun op
    | head op `elem` "+-*/" && length op > 1 =
        let n = read (tail op) :: Double
        in Function 1 (\[x] -> Numerical (applyCustomOp (head op) (toNum x) n))
    | otherwise = error ("Unknown function operator: " ++ op)
  where
    applyCustomOp '+' = (+)
    applyCustomOp '-' = (-)
    applyCustomOp '*' = (*)
    applyCustomOp '/' = (/)
    applyCustomOp _   = error "Invalid operator"

eqValue :: Value -> Value -> Bool
eqValue (Numerical a) (Numerical b)     = a == b
eqValue (Vectorial xs) (Vectorial ys) 
  | length xs == length ys              = and (zipWith eqValue xs ys)
  | otherwise                           = False
eqValue _ _                             = False

extractNums :: Value -> [Double]
extractNums (Numerical n)  = [n]  -- Wrap single numbers in a list
extractNums (Vectorial xs) = concatMap extractNums xs
extractNums _              = error "Expected a numerical vector"

reduceFunc :: Expression -> Expression -> Value
reduceFunc fExpr arrExpr =
    case eval fExpr of
        Function 2 f -> case eval arrExpr of
            Vectorial xs -> Numerical (foldl (\acc x -> toNum (f [Numerical acc, x])) 0 xs)
            _ -> error "Reduce expects a vector as its second argument"
        _ -> error "Reduce expects a binary function"

mapFunc :: Expression -> Expression -> Value
mapFunc fExpr arrExpr =
    case eval fExpr of
        Function 1 f -> case eval arrExpr of
            Vectorial xs -> f [Vectorial xs]
            _ -> error "Map expects a vector as its second argument"
        _ -> error "Map expects a function as its first argument"

negValue :: Value -> Value
negValue (Numerical n) = Numerical (-n)
negValue (Vectorial xs) = Vectorial (map negValue xs)
negValue _ = error "Negation expects a numerical value"

safeDiv :: Double -> Double -> Double
safeDiv x y
    | y == 0 = error "Division by zero"
    | otherwise = x / y

rangeFunc :: Double -> Value
rangeFunc x = Vectorial (map (Numerical . fromIntegral) [1 .. floor x])

applyUnary :: (Value -> Value) -> Value -> Value
applyUnary f (Vectorial xs) = Vectorial (map f xs)
applyUnary f v              = f v