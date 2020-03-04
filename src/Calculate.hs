module Calculate (derive) where
import DataStructures
import Prelude hiding (exp)

manyStep :: [Law] -> Expr -> [Step]
manyStep ls e = case steps of
                    [] -> []
                    (Step name exp):_ -> (Step name exp) : (manyStep ls exp)
                where steps = makeStep ls e

derive :: [Law] -> Result Expr -> Result Calculation
derive ls (Correct e) = Correct (Calculation e (manyStep ls e))
derive _ (Error str) = Error str

makeStep :: [Law] -> Expr -> [Step]
makeStep ls e = [Step name res | Law name (e1, e2) <- ls, res <- (map manyArithmetic (putItTogether e1 e2 e))]

putItTogether :: Expr -> Expr -> Expr -> [Expr]
putItTogether e1 e2 exp
   = [apply sub e2 | sub <- match e1 exp] ++
     case exp of
         (BinOp op left right) -> [BinOp op left' right | left' <- putItTogether e1 e2 left] ++
                                  [BinOp op left right' | right' <- putItTogether e1 e2 right]
         (Unary op e) -> [Unary op exp' | exp' <- putItTogether e1 e2 e]
         (Deriv var e) -> [Deriv var exp' | exp' <- putItTogether e1 e2 e]
         _ -> []

manyArithmetic :: Expr -> Expr
manyArithmetic exp
    | exp == (arithmetic exp) = exp
    | otherwise = manyArithmetic (arithmetic exp)

arithmetic :: Expr -> Expr
arithmetic (BinOp op (Const a) (Const b)) = Const((getOp op) a b)
arithmetic (BinOp op left right) = BinOp op (arithmetic left) (arithmetic right)
arithmetic (Unary op exp) = Unary op (arithmetic exp)
arithmetic (Deriv var exp) = Deriv var (arithmetic exp)
arithmetic exp = exp

getOp :: BOp -> (Int -> Int -> Int)
getOp op
    | op == Add = (+)
    | op == Mul = (*)
    | op == Div = div
    | op == Sub = (-)
    | otherwise = (^)

apply :: Subst -> Expr -> Expr
apply subst (BinOp op left right) = BinOp op (apply subst left) (apply subst right)
apply subst (Unary op exp) = Unary op (apply subst exp)
apply subst (Deriv var exp) = Deriv (apply subst var) (apply subst exp)
apply ((v,e):subst) (Var c)
    | v == (Var c) = e
    | otherwise = apply subst (Var c)
apply [] x = x
apply _ (Const i) = (Const i)


match :: Expr -> Expr -> [Subst]
match (Deriv varL expL) (Deriv varE expE) = [l ++ r | l<-match varL varE, r<-match expL expE,compatible l r]
match (BinOp opL leftL rightL) (BinOp opE leftE rightE)
    | opL == opE = [l ++ r | l<-match leftL leftE, r<- match rightL rightE,compatible l r]
    | otherwise = []
match (Unary opL expL) (Unary opE expE)
    | opL == opE = match expL expE
    | otherwise = []
match (Var 'z') (Const i) = [[((Var 'z'), (Const i))]]
match (Var 'z') _ = []
match (Var l) exp = [[(Var l, exp)]]
match (Const i) (Const a)
    | i==a = [[((Const i), (Const i))]]
    | otherwise = []
match _ _ = []

compatible :: Subst -> Subst -> Bool
compatible (((var1, exp1)):_) (((var2, exp2)):_)
    | var1 /= var2 = True
    | (var1, exp1) == (var2, exp2) = True
    | otherwise = False
compatible _ _ = True