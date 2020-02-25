module DataStructures where
import Text.Megaparsec
import Data.Void(Void)

data Law = Law LawName Equation deriving (Show, Eq)
type LawName = String
type Equation = (Expr, Expr)

data Calculation = Calculation Expr [Step] deriving (Show, Eq)
data Step = Step LawName Expr deriving (Show, Eq)

data BOp = Add | Mul | Div | Sub | Pow deriving (Show, Eq)
data UOp = Sin | Cos | Ln | Negation deriving (Show, Eq)
data Expr = BinOp BOp Expr Expr
            | Unary UOp Expr
            | Deriv Expr Expr
            | Var Char
            | Const Int
            deriving (Show, Eq)

type Parser = Parsec Void String 
data Result a = Correct a | Error String deriving (Show, Eq)
