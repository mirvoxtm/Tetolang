module Returns where

data Value = Numerical Double
           | Vectorial [Value]
           | Function { arity :: Int, fun :: [Value] -> Value }


instance Show Value where
    show (Numerical n) = show n
    show (Vectorial xs) = "[" ++ inner xs ++ "]"
        where
          inner [] = ""
          inner [v] = show v
          inner (v:vs) = show v ++ ", " ++ inner vs
    show (Function _ _) = "<function>"

toNum :: Value -> Double
toNum (Numerical n) = n
toNum _ = error "Expected a numerical value"

(+++) :: Value -> Value -> Value
(+++) (Numerical x) (Numerical y) = Numerical (x + y)
(+++) (Vectorial x) (Vectorial y) = Vectorial (x ++ y)
(+++) (Numerical x) (Vectorial y) = Vectorial (map (\v -> Numerical (x + toNum v)) y)
(+++) (Vectorial x) (Numerical y) = Vectorial (x ++ [Numerical y])

(-/-) :: Value -> Value -> Value
(-/-) (Numerical x) (Numerical y) = Numerical (x - y)
(-/-) (Vectorial x) (Vectorial y)
  | length x == length y = Vectorial (zipWith (\a b -> Numerical (toNum a - toNum b)) x y)
  | otherwise = Vectorial []
(-/-) (Numerical x) (Vectorial y) = Vectorial (map (\v -> Numerical (x - toNum v)) y)
(-/-) (Vectorial x) (Numerical y) = Vectorial (reverse (drop (floor y) (reverse x)))

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
