module Calculate (derive) where
import DataStructures
import Laws
import Prelude hiding (exp)

manyStep :: [Law] -> Expr -> [Step]
manyStep ls e = case steps of
                    [] -> []
                    (Step name exp):_ -> (Step name exp) : (manyStep ls exp)
                where steps = makeStep ls e

derive :: [Law] -> Expr -> Calculation
derive ls e = Calculation e (manyStep ls e)

makeStep :: [Law] -> Expr -> [Step]
makeStep ls e = [Step name res | Law name (e1, e2) <- ls, res <- (putItTogether e1 e2 e)]

putItTogether :: Expr -> Expr -> Expr -> [Expr]
putItTogether e1 e2 exp
   = [apply sub e2 | sub <- match e1 exp] ++
     case exp of
         (BinOp op left right) -> [BinOp op left' right | left' <- putItTogether e1 e2 left] ++
                                  [BinOp op left right' | right' <- putItTogether e1 e2 right]
         (Unary op exp) -> [Unary op exp' | exp' <- putItTogether e1 e2 exp]
         (Deriv var exp) -> [Deriv var exp' | exp' <- putItTogether e1 e2 exp]
         _ -> []

{-No longer used
insertE2 :: Expr -> Expr -> Expr -> Expr
insertE2 (BinOp op left right) modifiedE2 partExp
    | partExp == (BinOp op left right) = modifiedE2
    | otherwise = BinOp op (insertE2 left modifiedE2 partExp) (insertE2 right modifiedE2 partExp)
insertE2 (Unary op exp) modifiedE2 partExp
    | partExp == (Unary op exp) = modifiedE2
    | otherwise = Unary op (insertE2 exp modifiedE2 partExp)
insertE2 (Deriv var exp) modifiedE2 partExp
    | partExp == (Deriv var exp) = modifiedE2
    | otherwise = Deriv var (insertE2 exp modifiedE2 partExp)
insertE2 (Var c) modifiedE2 partExp
    | partExp == (Var c) = modifiedE2
    | otherwise = Var c
insertE2 (Const i) modifiedE2 partExp
    | partExp == (Const i) = modifiedE2
    | otherwise = Const i-}

apply :: Subst -> Expr -> Expr
apply subst (BinOp op left right) = BinOp op (apply subst left) (apply subst right)
apply subst (Unary op exp) = Unary op (apply subst exp)
apply subst (Deriv var exp) = Deriv (apply subst var) (apply subst exp)
apply ((v,e):subst) (Var c)
    | v == (Var c) = e
    | otherwise = apply subst (Var c)
apply [] x = x
apply _ (Const i) = (Const i)

{-No longer used
removeBadEntries :: [(Expr, [(Expr, Expr)])] -> [(Expr, [(Expr, Expr)])]
removeBadEntries [] = []
removeBadEntries ((main, ls):subs)
    | length ls == 0 = removeBadEntries subs
    | otherwise = (main, ls) : removeBadEntries subs-}

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

-- test structures
testExp :: Expr
testExp = (Deriv (Var 'x') (BinOp Div (Var 'x') (Const 2)))

testE1 :: Expr
testE1 = (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')))

testE2 :: Expr
testE2 = BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b')))

testEqn :: Equation
testEqn = (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')), BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b'))))

testPart :: Expr
testPart = (Deriv (Var 'x') (BinOp Mul (Const 2) (Unary Ln (Var 'x'))))

testSubs :: Subst
testSubs = [(Var 'x',Var 'x'),(Var 'a',Const 2),(Var 'b',Unary Ln (Var 'x'))]