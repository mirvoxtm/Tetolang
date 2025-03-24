module Parser.Ast where
    data Expression = Num Double
                    | Sum Expression Expression
                    | Sub Expression Expression
                    | Mul Expression Expression
                    | Div Expression Expression
                    | Range Expression
                    | Max Expression
                    | Arr [Expression]
                    | Fun String
                    | Reduce Expression Expression
                deriving (Show, Eq)