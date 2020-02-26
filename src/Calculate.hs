module Calculate where
import DataStructures
import Laws
import Prelude hiding (exp)

-- Function does not work as aspected: Does not terminate
--calculate :: [Law] -> Expr -> Calculation
--calculate ls e = Calculation e (manyStep rws e)
    --where rws a = [Step name e' | Law name eq <- ls, e' <- rewrites eq a]

-- Function does not work as expected: Does not terminate
manyStep :: [Law] -> Expr -> [Step]
manyStep ls e = case steps of
                    [] -> []
                    (Step name exp):_ -> (Step name exp) : (manyStep ls exp)
                where steps = makeStep ls e

derive :: [Law] -> Expr -> Calculation
derive ls e = Calculation e (manyStep ls e)

makeStep :: [Law] -> Expr -> [Step]
makeStep ls e = [Step name (applyAll (getSubs e1 e) e2) | Law name (e1, e2) <- ls, length (getSubs e1 e) /= 1]

-- Function only returns lists of size 1, which is probably not right
rewrites :: Equation -> Expr -> Expr
rewrites (e1, e2) exp = applyAll (getSubs e1 exp) e2

-- Applies all possible substitutions, which is probably not always right
applyAll :: [(Expr, Expr)] -> Expr -> Expr
applyAll [] e = e
applyAll ((v, e):subs) e2 = applyAll subs (apply (v, e) e2)

testSubs :: [(Expr, Expr)]
testSubs = [(Var 'x',Var 'x'),(Var 'a',Const 2),(Var 'b',Unary Ln (Var 'x'))]

apply :: (Expr, Expr) -> Expr -> Expr
apply (v, e) (BinOp op left right) = BinOp op (apply (v, e) left) (apply (v, e) right)
apply (v, e) (Unary op exp) = Unary op (apply (v, e) exp)
apply (v, e) (Deriv var exp) = Deriv (apply (v, e) var) (apply (v, e) exp)
apply (v, e) (Var c)
    | v == (Var c) = e
    | otherwise = (Var c)
apply _ (Const i) = (Const i)

getSubs :: Expr -> Expr -> [(Expr, Expr)]
getSubs e1 (BinOp op left right) = match e1 (BinOp op left right) ++
                                getSubs e1 left ++ getSubs e1 right
getSubs e1 (Unary op exp) = match e1 (Unary op exp) ++ getSubs e1 exp
getSubs e1 (Deriv var exp) = match e1 (Deriv var exp) ++ 
                                getSubs e1 var ++ getSubs e1 exp
getSubs e1 (Var c) = match e1 (Var c) ++ []
getSubs e1 (Const i) = match e1 (Const i) ++ []

match :: Expr -> Expr -> [(Expr, Expr)]
match (Deriv varL expL) (Deriv varE expE) = match varL varE ++ match expL expE
match (BinOp opL leftL rightL) (BinOp opE leftE rightE)
    | opL == opE = match leftL leftE ++ match rightL rightE
    | otherwise = []
match (Unary opL expL) (Unary opE expE)
    | opL == opE = match expL expE
    | otherwise = []
match (Var l) exp = [(Var l, exp)]
match _ _ = []

-- test structures
testExp :: Expr
testExp = (BinOp Mul (BinOp Pow (Var 'x') (Const 2)) (Deriv (Var 'x') (BinOp Mul (Const 2) (Unary Ln (Var 'x')))))

testE1 :: Expr
testE1 = (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')))

testE2 :: Expr
testE2 = BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b')))

testEqn :: Equation
testEqn = (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')), BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b'))))
