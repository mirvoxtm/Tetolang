module Parser.Ast where
    data Expression = Num Double
        | Sum Expression Expression
        | Sub Expression Expression
        | Mul Expression Expression
        | Div Expression Expression
        | Range Expression
        | Arr [Expression]
        | Fun String
        | Map Expression Expression
        | Reduce Expression Expression
        | Max Expression
        | Min Expression
        deriving (Show, Eq)
