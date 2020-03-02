module DataStructures where
import Text.Megaparsec
import Data.Void(Void)
import Prelude hiding (exp)

data Law = Law LawName Equation deriving Eq
type LawName = String
type Equation = (Expr, Expr)

data Calculation = Calculation Expr [Step] deriving Eq
data Step = Step LawName Expr deriving Eq
type Subst = [(Expr, Expr)]

data BOp = Add | Mul | Div | Sub | Pow deriving Eq
data UOp = Sin | Cos | Ln | Negation deriving Eq
data Expr = BinOp BOp Expr Expr
            | Unary UOp Expr
            | Deriv Expr Expr
            | Var Char
            | Const Int
            deriving Eq

type Parser = Parsec Void String 
data Result a = Correct a | Error String deriving (Show, Eq)

-- show instances

instance Show Expr where
    show (BinOp bop expL expR) = "(" ++ (show expL) ++ (show bop) ++ (show expR) ++ ")"
    show (Unary uop exp) = (show uop) ++ "(" ++ (show exp) ++ ")"
    show (Deriv var exp) = "(deriv " ++ (show var) ++ ") " ++ (show exp)
    show (Var c) = [c]
    show (Const i) = show i

instance Show BOp where
    show Add = " + "
    show Mul = " * "
    show Div = "/"
    show Sub = " - "
    show Pow = "^"

instance Show UOp where
    show Sin = "sin "
    show Cos = "cos "
    show Ln = "ln "
    show Negation = "-"

instance Show Law where
    show (Law name (e1, e2)) = (show name) ++ ": " ++ (show e1) ++ " = " ++ (show e2)

instance Show Step where
    show (Step name exp) = "= {" ++ (show name) ++ "}\n" ++ (show exp) ++ "\n"

instance Show Calculation where
    show (Calculation exp steps) = (show exp) ++ "\n" ++ (concatMap (show) (steps))