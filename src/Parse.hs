module Parse (parseInputExpr, parseInputLaw) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import DataStructures

-- Earlier in the list = stronger binding
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  map (:[]) [ prefix "sin" (Unary Sin)
    , prefix "cos" (Unary Cos)
    , prefix "ln" (Unary Ln)
    , prefix "-" (Unary Negation)
    , prefix "+" id
    ] ++
  [
    [ binary "^" (BinOp Pow)]
  ,
    [ binary "*" (BinOp Mul)
    , binary "/" (BinOp Div)
    ]
  , [ binary "+" (BinOp Add)
    , binary "-" (BinOp Sub)
    ]
  ]

-- Parse out space and comment chars
sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

-- Calls space and comment parser
symbol :: String -> Parser String
symbol = L.symbol sc

-- Binary operators are parsed here
binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

-- Unary operators are parsed here
prefix:: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (foldr1 (.) <$> some (f <$ symbol name))

-- Takes an input string and parses it into the Expr data type
parseInputExpr :: String -> Result Expr
parseInputExpr p = case parse (parseExpr <* eof) "" p of
                      Left e -> Error (errorBundlePretty e)
                      Right x -> Correct x

-- Takes an input string and parses it into the Law data type.
-- List is always of size 1 or 0. Errors are thrown out for ease of use.
parseInputLaw :: String -> [Law]
parseInputLaw p = case parse (parseLaw <* eof) "" p of
                      Left _ -> []
                      Right x -> [x]

-- Creates expression parser using megaparsec
parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable

-- Parses a string into the Expr datatype
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
                d <- some digitChar;
                _ <- space;
                return (Const (read d))}) <|>
            (try $ do  {_ <- space;
                v <- letterChar;
                _ <- notFollowedBy letterChar;
                _ <- space;
                return (Var v)})

-- Parses a string into the Law datatype
parseLaw :: Parser Law
parseLaw = do {_ <- space;
               lawName <- many (alphaNumChar <|> spaceChar <|> char '-');
               _ <- space;
               _ <- string ":";
               _ <- space;
               left <- parseExpr;
               _ <- space;
               _ <- string "=";
               _ <- space;
               right <- parseExpr;
               return (Law lawName (left, right))}
               