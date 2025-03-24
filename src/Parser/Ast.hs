module Parser.Ast where
    data Expression
        = Num Double
        | Sum Expression Expression
        | Sub Expression Expression
        | Mul Expression Expression
        | Div Expression Expression
        | Exp Expression Expression
        | Fact Expression
        | Neg Expression
        | Range Expression
        | Max Expression
        | Min Expression
        | Id Expression
        | Reduce Expression Expression
        | Map Expression Expression
        | Arr [Expression]
        | Fun String
        | Reverse Expression
        | Rotate Expression Expression
        | Index Expression Expression
        | Match Expression Expression
        | Mod Expression Expression

        deriving (Show, Eq)
