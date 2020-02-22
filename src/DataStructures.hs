module DataStructures where
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor.Identity (Identity)

-- Data Structures
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

-- Parsing
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" (Unary Negation)
    , prefix "+" id
    , prefix "sin" (Unary Sin)
    , prefix "cos" (Unary Cos)
    , prefix "ln" (Unary Ln)
    ]
  ,
    [ binary "*" (BinOp Mul)
    , binary "/" (BinOp Div)
    ]
  , [ binary "+" (BinOp Add)
    , binary "-" (BinOp Sub)
    ]
  ]

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

symbol :: String -> Parser String
symbol = L.symbol sc

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

type Parser = Parsec Void String 

parseInput p = parseTest (parseExpr <* eof) p


parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable
{-
parseExpr :: Parser Expr
parseExpr = parseTerm >>= rest
    where rest e1 = (do {_ <- space;
                        o <- parseBOp;
                        _ <- space;
                        e2 <- parseExpr;
                        return (BinOp o e1 e2)}) <|>
                    return e1
-}
parseTerm :: Parser Expr
parseTerm = do  {_ <- space;
                _ <- string "(";
                _ <- space;
                e <- parseExpr;
                _ <- space;
                _ <- string ")";
                _ <- space;
                return e} <|>
            do  {_ <- space;
                u <- parseUnary;
                _ <- space;
                return u} <|>
            do  {_ <- space;
                _ <- string "deriv";
                _ <- space;
                v <- letterChar;
                _ <- space;
                e <- parseExpr;
                _ <- space;
                return (Deriv (Var v) e)} <|>
            do  {_ <- space;
                d <- some digitChar;
                _ <- space;
                return (Const (read d))} <|>
            do  {_ <- space;
                v <- letterChar;
                _ <- space;
                return (Var v)}

parseUnary :: Parser Expr
parseUnary = do {utype <- parseUOp;
                _ <- space;
                arg <- parseExpr;
                return (Unary utype arg)}

parseUOp :: Parser UOp
parseUOp =  do  {_ <- string "sin";
                return Sin} <|>
            do  {_ <- string "cos";
                return Cos} <|>
            do  {_ <- string "ln";
                return Ln} <|>
            do  {_ <- string "-";
                return Negation} 

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