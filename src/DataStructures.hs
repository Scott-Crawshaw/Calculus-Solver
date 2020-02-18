module DataStructures where
    
-- Data structues taken from chapter 12 of Thinking Functionally with Haskell by Richard Bird

-- Data Structures
data Law = Law LawName Equation deriving Show
type LawName = String
type Equation = (Expr, Expr)

data Expr = Expr [Term] deriving Show
data Term = Var VarName | Const ConstName | Op OpType | Paren ParenSide deriving Show
type VarName = String
type ConstName = String
type OpType = String
type ParenSide = String

data Calculation = Calculation Expr [Step] deriving Show
type Step = (LawName, Expr)

-- Example
exampleLaw = Law "Derivative of a Constant is Zero" (Expr [Op "d/dx", Const "a"], Expr [Const "0"])
exampleCalculation = Calculation (Expr [Op "d/dx", Const "5"]) [("Derivative of a Constant is Zero", Expr [Const "0"])]