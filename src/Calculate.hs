module Calculate where
import DataStructures
import Laws

--calculate :: [Law] -> Expr -> Calculation
--calculate laws e = Calculation e (manyStep rws e)
    --where rws e = [Step name e' | Law name eq <- laws, e' <- rewrites eq e]

--manyStep :: (Expr -> [Step]) -> Expr -> [Step]
--manyStep rws e = case steps of
                    --[] -> []
                    --(o@(Step _ e):_) -> o:manyStep rws e
                --where steps = rws e

rewrites :: Equation -> Expr -> [(Expr, Expr)]
rewrites (e1, e2) (BinOp op left right) = match e1 (BinOp op left right) ++
                                rewrites (e1, e2) left ++ rewrites (e1, e2) right
rewrites (e1, e2) (Unary op exp) = match e1 (Unary op exp) ++ rewrites (e1, e2) exp
rewrites (e1, e2) (Deriv var exp) = match e1 (Deriv var exp) ++ 
                                rewrites (e1, e2) var ++ rewrites (e1, e2) exp
rewrites (e1, e2) (Var c) = match e1 (Var c) ++ []
rewrites (e1, e2) (Const i) = match e1 (Const i) ++ []

testExp :: Expr
testExp = Deriv (Var 'x') (BinOp Add (Var 'x') (Var 'y'))

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
match (Var l) (Var e) = [(Var l, Var e)]
match (Const l) (Const e) = [(Const l, Const e)]
match _ _ = []
