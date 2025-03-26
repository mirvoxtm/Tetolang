module Returns where
import Data.List

data Value = Numerical Double
           | Vectorial [Value]
           | Booleanical Bool
           | Characterial Char
           | Function { arity :: Int, fun :: [Value] -> Value }

instance Eq Value where
    Numerical x == Numerical y = x == y
    Vectorial xs == Vectorial ys = xs == ys
    Booleanical x == Booleanical y = x == y
    Characterial x == Characterial y = x == y
    Function _ _ == Function _ _ = False
    _ == _ = False

instance Show Value where
  show (Vectorial xs)
    | all isChar xs = "\"" ++ map getChar xs ++ "\""
    | otherwise     = "[" ++ inner xs ++ "]"
    where
      inner []     = ""
      inner [v]    = show v
      inner (v:vs) = show v ++ ", " ++ inner vs
      isChar (Characterial _) = True
      isChar _                = False
      getChar (Characterial c) = c
  show (Numerical n)    = show n
  show (Booleanical b)  = if b then "O" else "X"
  show (Characterial c) = [c]
  show (Function _ _)   = "<function>"

toNum :: Value -> Double
toNum (Numerical n) = n
toNum _ = error "Expected a numerical value"

removeSubstring :: String -> String -> String
removeSubstring needle haystack
  | null needle = haystack
  | otherwise   = go haystack
  where
    n = length needle
    go [] = []
    go str | needle `isPrefixOf` str = go (drop n str)
           | otherwise             = head str : go (tail str)


(+++) :: Value -> Value -> Value
(+++) (Numerical x) (Numerical y) = Numerical (x + y)
(+++) (Vectorial x) (Vectorial y) = Vectorial (x ++ y)
(+++) (Numerical x) (Vectorial y) = Vectorial (map (\v -> Numerical (x + toNum v)) y)
(+++) (Vectorial x) (Numerical y) = Vectorial (x ++ [Numerical y])
(+++) (Numerical x) (Characterial y) = Vectorial ([Characterial y] ++ [Characterial y])
(+++) _ _ = error "Invalid types for concatenation"

(-/-) :: Value -> Value -> Value
(-/-) (Numerical x) (Numerical y) = Numerical (x - y)
(-/-) (Vectorial x) (Vectorial y)
  | length x == length y = Vectorial (zipWith (\a b -> Numerical (toNum a - toNum b)) x y)
  | otherwise = Vectorial []
(-/-) (Numerical x) (Vectorial y) = Vectorial (map (\v -> Numerical (x - toNum v)) y)
(-/-) (Vectorial x) (Numerical y) = Vectorial (reverse (drop (floor y) (reverse x)))
(-/-) _ _ = error "Invalid types for subtraction"

(*/*) :: Value -> Value -> Value
(*/*) (Numerical x) (Numerical y) = Numerical (x * y)
(*/*) (Vectorial x) (Vectorial y)
  | length x == length y = Vectorial (zipWith (\a b -> Numerical (toNum a * toNum b)) x y)
  | otherwise =
      let maxLen = max (length x) (length y)
          xs = take maxLen (cycle x)
          ys = take maxLen (cycle y)
      in Vectorial (zipWith (\a b -> Numerical (toNum a * toNum b)) xs ys)
(*/*) (Numerical x) (Vectorial ys) = Vectorial (map (\v -> Numerical (x * toNum v)) ys)
(*/*) (Vectorial xs) (Numerical n) = Vectorial (map Vectorial (replicate (round n) xs))
(*/*) (Characterial x) (Numerical y) = Vectorial (replicate (round y) (Characterial x))
(*/*) (Numerical x) (Characterial y) = Vectorial (replicate (round x) (Characterial y))
(*/*) _ _ = error "Invalid types for multiplication"

(/|\) :: Value -> Value -> Value
(/|\) (Numerical x) (Numerical y) = Numerical (x / y)
(/|\) (Vectorial x) (Vectorial y)
  | length x == length y = Vectorial (zipWith (\a b -> Numerical (toNum a / toNum b)) x y)
  | otherwise =
      let maxLen = max (length x) (length y)
          xs = take maxLen (cycle x)
          ys = take maxLen (cycle y)
      in Vectorial (zipWith (\a b -> Numerical (toNum a / toNum b)) xs ys)
(/|\) (Numerical x) (Vectorial ys) = Vectorial (map (\v -> Numerical (x / toNum v)) ys)
(/|\) (Vectorial xs) (Numerical n) = Vectorial (map (\v -> Numerical (toNum v / n)) xs)
(/|\) (Characterial x) (Numerical y) = Vectorial (replicate (round y) (Characterial x))
(/|\) (Numerical x) (Characterial y) = Vectorial (replicate (round x) (Characterial y))
(/|\) _ _ = error "Invalid types for division"

(^/^) :: Value -> Value -> Value
(^/^) (Numerical x) (Numerical y) = Numerical (x ** y)
(^/^) (Vectorial x) (Vectorial y)
  | length x == length y = Vectorial (zipWith (\a b -> Numerical (toNum a ** toNum b)) x y)
  | otherwise =
      let maxLen = max (length x) (length y)
          xs = take maxLen (cycle x)
          ys = take maxLen (cycle y)
      in Vectorial (zipWith (\a b -> Numerical (toNum a ** toNum b)) xs ys)
(^/^) (Numerical x) (Vectorial ys) = Vectorial (map (\v -> Numerical (x ** toNum v)) ys)
(^/^) (Vectorial xs) (Numerical n) = Vectorial (map (\v -> Numerical (toNum v ** n)) xs)
(^/^) (Characterial x) (Numerical y) = Vectorial (replicate (round y) (Characterial x))
(^/^) (Numerical x) (Characterial y) = Vectorial (replicate (round x) (Characterial y))
(^/^) _ _ = error "Invalid types for exponentiation"

isNumerical :: Value -> Bool
isNumerical (Numerical _) = True
isNumerical _             = False

isVector :: Value -> Bool
isVector (Vectorial _) = True
isVector _             = False

minIfVector :: Value -> Value
minIfVector (Vectorial xs)
  | all isNumerical xs = Numerical (minimum (map toNum xs))
  | all isVector xs    = Vectorial (map minIfVector xs)
  | otherwise          = error "Min function expects a uniform vector"
minIfVector v = v

maxIfVector :: Value -> Value
maxIfVector (Vectorial xs) =
  case concatMap extractNumerical xs of
    []  -> error "Max function expects at least one numerical value"
    nums -> Numerical (maximum nums)
  where
    extractNumerical :: Value -> [Double]
    extractNumerical (Numerical n) = [n]
    extractNumerical (Vectorial ys) = concatMap extractNumerical ys
    extractNumerical _ = error "Max function expects a uniform vector"

maxIfVector _ = error "Max function expects a vector"
