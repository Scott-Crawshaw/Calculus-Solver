module DataStructures where
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)


-- Data structues taken from chapter 12 of Thinking Functionally with Haskell by Richard Bird

-- Data Structures
data Law = Law LawName Equation deriving Show
type LawName = String
type Equation = (Expr, Expr)

data Calculation = Calculation Expr [Step] deriving Show
type Step = (LawName, Expr)

data BOp = Add | Mul | Div | Sub | Pow deriving Show
data UOp = Sin | Cos | Ln deriving Show
data Expr = BinOp BOp Expr Expr
            | Unary UOp Expr
            | Const Int
            deriving Show
-- Var String
-- Deriv String Expr

-- Parsing
type Parser = Parsec Void String 

parseExpr :: Parser Expr
parseExpr = term >>= rest
    where rest e1 = (do  {o <- parseBOp;
                        e2 <- parseExpr;
                        return (BinOp o e1 e2)}) <|>
                    return e1

term :: Parser Expr
term =  do  {d <- some digitChar;
            return (Const (read d))} <|>
        do  {_ <- string "(";
            e <- parseExpr;
            _ <- string ")";
            return e} <|>
        do  {u <- parseUnary;
            return u}

parseUnary :: Parser Expr
parseUnary = do {utype <- parseUOp;
                arg <- parseExpr;
                return (Unary utype arg)}

parseUOp :: Parser UOp
parseUOp =  do  {_ <- string "sin";
                return Sin} <|>
            do  {_ <- string "cos";
                return Cos} <|>
            do  {_ <- string "ln";
                return Ln} 

parseBOp :: Parser BOp
parseBOp =  do  {_ <- string "*";
                return Mul} <|>
            do {_ <- string "/";
                return Div} <|>
            do  {_ <- string "+";
                return Add} <|>
            do  {_ <- string "-";
                return Sub} <|>
            do  {_ <- string "^";
                return Pow} 


        

-- Example
{-
exampleLaw = Law "Derivative of a Constant is Zero" (Expr [Op "d/dx", Const "a"], Expr [Const "0"])
exampleCalculation = Calculation (Expr [Op "d/dx", Const "5"]) [("Derivative of a Constant is Zero", Expr [Const "0"])]
-}