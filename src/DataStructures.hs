module DataStructures where
import Text.Megaparsec
import Data.Void(Void)

data Law = Law LawName Equation deriving Show
type LawName = String
type Equation = (Expr, Expr)

data Calculation = Calculation Expr [Step] deriving Show
type Step = (LawName, Expr)

data BOp = Add | Mul | Div | Sub | Pow deriving Show
data UOp = Sin | Cos | Ln | Negation deriving Show
data Expr = BinOp BOp Expr Expr
            | Unary UOp Expr
            | Deriv Expr Expr
            | Var Char
            | Const Int
            deriving Show

type Parser = Parsec Void String 
data Result a = Correct a | Error String deriving Show
