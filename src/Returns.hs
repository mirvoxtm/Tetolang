module Returns where

data Value = Numerical Double
           | Vectorial [Value]
           | Function { arity :: Int, 
                        fun :: [Double] -> Double
                      }

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
(-/-) (Vectorial x) (Vectorial y) = Vectorial []
(-/-) (Numerical x) (Vectorial y) = Vectorial (map (\v -> Numerical (x - toNum v)) y)
(-/-) (Vectorial x) (Numerical y) = Vectorial (reverse (drop (floor y) (reverse x)))

(*/*) :: Value -> Value -> Value
(*/*) (Numerical x) (Numerical y) = Numerical (x * y)
(*/*) (Vectorial x) (Vectorial y)
  | length x == length y = VeWctorial (zipWith (\a b -> Numerical (toNum a * toNum b)) x y)
  | otherwise = error "Vector lengths must match for element-wise multiplication"
(*/*) (Numerical x) (Vectorial ys) = Vectorial (map (\v -> Numerical (x * toNum v)) ys)
(*/*) (Vectorial xs) (Numerical n) = Vectorial (map Vectorial (replicate (round n) xs))

(/|\) :: Value -> Value -> Value
(/|\) (Numerical x) (Numerical y) = Numerical (x / y)
(/|\) (Vectorial x) (Vectorial y)
  | length x == length y = Vectorial (zipWith (\a b -> Numerical (toNum a / toNum b)) x y)
  | otherwise = error "Vector lengths must match for element-wise division"
(/|\) (Numerical x) (Vectorial ys) = Vectorial (map (\v -> Numerical (x / toNum v)) ys)
(/|\) (Vectorial xs) (Numerical n) = Vectorial (map (\v -> Numerical (toNum v / n)) xs)