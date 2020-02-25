module Calculate where
import DataStructures
import Laws

calculate :: [Law] -> Expr -> Calculation
calculate ls e = Calculation e (manyStep rws e):q
    where rws e = [Step name e' | Law name eq <- ls, e' <- rewrites eq e]

manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e = case steps of
                    [] -> []
                    (o@(Step _ e):_) -> o:manyStep rws e
                where steps = rws e

rewrites :: Equation -> Expr -> [Expr]
rewrites (e1, e2) exp = [applyAll (getSubs e1 exp) e2]

applyAll :: [(Expr, Expr)] -> Expr -> Expr
applyAll [] e = e
applyAll ((v, e):subs) e2 = applyAll subs (apply (v, e) e2)

apply :: (Expr, Expr) -> Expr -> Expr
apply (v, e) (BinOp op left right) = BinOp op (apply (v, e) left) (apply (v, e) right)
apply (v, e) (Unary op exp) = Unary op (apply (v, e) exp)
apply (v, e) (Deriv var exp) = Deriv (apply (v, e) var) (apply (v, e) exp)
apply (v, e) (Var c)
    | v == (Var c) = e
    | otherwise = (Var c)
apply (v, e) (Const i)
    | v == (Const i) = e
    | otherwise = (Const i)

getSubs :: Expr -> Expr -> [(Expr, Expr)]
getSubs e1 (BinOp op left right) = match e1 (BinOp op left right) ++
                                getSubs e1 left ++ getSubs e1 right
getSubs e1 (Unary op exp) = match e1 (Unary op exp) ++ getSubs e1 exp
getSubs e1 (Deriv var exp) = match e1 (Deriv var exp) ++ 
                                getSubs e1 var ++ getSubs e1 exp
getSubs e1 (Var c) = match e1 (Var c) ++ []
getSubs e1 (Const i) = match e1 (Const i) ++ []

testExp :: Expr
testExp = Deriv (Var 'x') (BinOp Add (Var 'x') (Const 4))

teste1 :: Expr
teste1 = Deriv (Var 'x') (BinOp Add (Var 'a') (Var 'b'))

testEqn :: Equation
testEqn = (Deriv (Var 'x') (BinOp Add (Var 'a') (Var 'b')), BinOp Add (Deriv (Var 'x') (Var 'a')) (Deriv (Var 'x') (Var 'b')))

match :: Expr -> Expr -> [(Expr, Expr)]
match (Deriv varL expL) (Deriv varE expE) = match expL expE
match (BinOp opL leftL rightL) (BinOp opE leftE rightE)
    | opL == opE = match leftL leftE ++ match rightL rightE
    | otherwise = []
match (Unary opL expL) (Unary opE expE)
    | opL == opE = match expL expE
    | otherwise = []
match (Var l) exp = [(Var l, exp)]
match (Const l) (Const e) = [(Const l, Const e)]
match _ _ = []
