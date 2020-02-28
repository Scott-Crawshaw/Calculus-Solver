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
makeStep ls e = [Step name res | Law name (e1, e2) <- ls, res <- (putItTogether e1 e2 e)]

-- Function only returns lists of size 1, which is probably not right

-- iSSUE (before meeting): should only be returning expression if expr 3 exists in expr 1
-- FIX: put it in a list so can return empty list
putItTogether :: Expr -> Expr -> Expr -> [Expr]
putItTogether e1 e2 exp
   = [apply sub e2 | sub <- match e1 exp] ++
     case exp of
         (BinOp op left right) -> [BinOp op left' right | left' <- putItTogether e1 e2 left] ++
                                  [BinOp op left right' | right' <- putItTogether e1 e2 right]
         -- TODO for other constructors (I did this part)
         (Unary op exp) -> [Unary op exp' | exp' <- putItTogether e1 e2 exp]
         (Deriv var exp) -> [Deriv var exp' | exp' <- putItTogether e1 e2 exp]
         -- base case for constant, variable
         _ -> []

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

testSubs :: Subst
testSubs = [(Var 'x',Var 'x'),(Var 'a',Const 2),(Var 'b',Unary Ln (Var 'x'))]

-- ideally takes String bc only going to be substituting for variables
apply :: Subst -> Expr -> Expr
apply subst (BinOp op left right) = BinOp op (apply subst left) (apply subst right)
apply subst (Unary op exp) = Unary op (apply subst exp)
apply subst (Deriv var exp) = Deriv (apply subst var) (apply subst exp)
apply ((v,e):subst) (Var c)
    | v == (Var c) = e
    | otherwise = apply subst (Var c)
apply [] x = x
apply _ (Const i) = (Const i)

removeBadEntries :: [(Expr, [(Expr, Expr)])] -> [(Expr, [(Expr, Expr)])]
removeBadEntries [] = []
removeBadEntries ((main, ls):subs)
    | length ls == 0 = removeBadEntries subs
    | otherwise = (main, ls) : removeBadEntries subs

-- ISSUE(before meeting w Joosten): returned empty list of substitutions
-- FIX: return empty list bc no possible sobstitutions
match :: Expr -> Expr -> [Subst]
match (Deriv varL expL) (Deriv varE expE) = [l ++ r | l<-match varL varE, r<-match expL expE,compatible l r]
match (BinOp opL leftL rightL) (BinOp opE leftE rightE)
    | opL == opE = [l ++ r | l<-match leftL leftE, r<- match rightL rightE,compatible l r]
    | otherwise = []
match (Unary opL expL) (Unary opE expE)
    | opL == opE = match expL expE
    | otherwise = []
match (Var l) exp = [[(Var l, exp)]]
match _ _ = []

compatible :: Subst -> Subst -> Bool
compatible _ _ = True -- TODO

-- test structures
testExp :: Expr
testExp = (Deriv (Var 'x') (BinOp Pow (Var 'x') (Const 2)))

testE1 :: Expr
testE1 = (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')))

testE2 :: Expr
testE2 = BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b')))

testEqn :: Equation
testEqn = (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')), BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b'))))

testPart :: Expr
testPart = (Deriv (Var 'x') (BinOp Mul (Const 2) (Unary Ln (Var 'x'))))
