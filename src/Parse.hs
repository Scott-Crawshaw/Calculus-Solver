module Parse where
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

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

-- Laws
derivativeOfSelf :: Law
derivativeOfSelf = Law "Derivative of Self" (Deriv (Var 'x') (Var 'x'), Const 1)

additionRule :: Law
additionRule = Law "Sum Rule" (Deriv (Var 'x') (BinOp Add (Var 'a') (Var 'b')), BinOp Add (Deriv (Var 'x') (Var 'a')) (Deriv (Var 'x') (Var 'b')))

productRule :: Law
productRule = Law "Product Rule" (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')), BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b'))))

derivSin :: Law
derivSin = Law "Derivative of Sin" (Deriv (Var 'x') (Unary Sin (Var 'a')), BinOp Mul (Unary Cos (Var 'a')) (Deriv (Var 'x') (Var 'a')))

derivCos :: Law
derivCos = Law "Derivative of Cos" (Deriv (Var 'x') (Unary Cos (Var 'a')), Unary Negation (Unary Sin (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'a')))))
-- Parsing
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "sin" (Unary Sin)
    , prefix "cos" (Unary Cos)
    , prefix "ln" (Unary Ln)
    , prefix "-" (Unary Negation)
    , prefix "+" id
    ]
  ,
    [ binary "^" (BinOp Pow)]
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

parseInput :: String -> IO()
parseInput p = parseTest (parseExpr <* eof) p


parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable

parseTerm :: Parser Expr
parseTerm = (try $ do  {_ <- space;
                _ <- string "(";
                _ <- space;
                e <- parseExpr;
                _ <- space;
                _ <- string ")";
                _ <- space;
                return e}) <|>
            (try $ do  {_ <- space;
                _ <- string "deriv";
                _ <- space;
                v <- letterChar;
                _ <- space;
                e <- parseExpr;
                _ <- space;
                return (Deriv (Var v) e)}) <|>
            (try $ do  {_ <- space;
                u <- parseUnary;
                _ <- space;
                return u}) <|>
            (try $ do  {_ <- space;
                d <- some digitChar;
                _ <- space;
                return (Const (read d))}) <|>
            do  {_ <- space;
                v <- letterChar;
                _ <- space;
                return (Var v)}

parseUnary :: Parser Expr
parseUnary = do {_ <- space;
                utype <- parseUOp;
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