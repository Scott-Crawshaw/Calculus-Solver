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
makeStep ls e = [Step name (putItTogether e1 e2 e) | Law name (e1, e2) <- ls, length (removeBadEntries (getSubs e1 e)) /= 0]

-- Function only returns lists of size 1, which is probably not right
rewrites :: Equation -> Expr -> Expr
rewrites (e1, e2) exp = applyAll (getSubs e1 exp) e2

callFuncs :: Expr -> Expr -> [(Expr, [(Expr, Expr)])] -> Expr
callFuncs exp e2 ((part, ls):xs) = insertE2 exp (helpApply ((part, ls):xs) e2) part

putItTogether :: Expr -> Expr -> Expr -> Expr
putItTogether e1 e2 exp = callFuncs exp e2 (removeBadEntries (getSubs e1 exp))

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
    | otherwise = Const i

helpApply :: [(Expr, [(Expr, Expr)])] -> Expr -> Expr
helpApply ((_, ls):_) e2 = writeE2 e2 ls

writeE2 :: Expr -> [(Expr, Expr)] -> Expr
writeE2 e2 [] = e2
writeE2 e2 ((v, e):subs) = writeE2 (apply (v,e) e2) subs

applyAll :: [(Expr, [(Expr, Expr)])] -> Expr -> Expr
applyAll [] e = e
applyAll ((_ , ((v, e):_)):subs) e2 = applyAll subs (apply (v, e) e2)

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

removeBadEntries :: [(Expr, [(Expr, Expr)])] -> [(Expr, [(Expr, Expr)])]
removeBadEntries [] = []
removeBadEntries ((main, ls):subs)
    | length ls == 0 = removeBadEntries subs
    | otherwise = (main, ls) : removeBadEntries subs

getSubs :: Expr -> Expr -> [(Expr, [(Expr, Expr)])]
getSubs e1 (BinOp op left right) = ((BinOp op left right), match e1 (BinOp op left right)) :
                                getSubs e1 left ++ getSubs e1 right
getSubs e1 (Unary op exp) = ((Unary op exp), match e1 (Unary op exp)) : getSubs e1 exp
getSubs e1 (Deriv var exp) = ((Deriv var exp), match e1 (Deriv var exp)) : 
                                getSubs e1 var ++ getSubs e1 exp
getSubs e1 (Var c) = ((Var c), match e1 (Var c)) : []
getSubs e1 (Const i) = ((Const i), match e1 (Const i)) : []

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

testPart :: Expr
testPart = (Deriv (Var 'x') (BinOp Mul (Const 2) (Unary Ln (Var 'x'))))
