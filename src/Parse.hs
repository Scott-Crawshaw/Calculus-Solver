module Parse (parseInputExpr, parseInputLaw) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import DataStructures

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

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

symbol :: String -> Parser String
symbol = L.symbol sc

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix:: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (foldr1 (.) <$> some (f <$ symbol name))

parseInputExpr :: String -> Result Expr
parseInputExpr p = case parse (parseExpr <* eof) "" p of
                      Left e -> Error (errorBundlePretty e)
                      Right x -> Correct x

parseInputLaw :: String -> [Law]
parseInputLaw p = case parse (parseLaw <* eof) "" p of
                      Left _ -> []
                      Right x -> [x]

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
                d <- some digitChar;
                _ <- space;
                return (Const (read d))}) <|>
            (try $ do  {_ <- space;
                v <- letterChar;
                _ <- notFollowedBy letterChar;
                _ <- space;
                return (Var v)})

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
               