module Laws (laws) where
import DataStructures

additionRule :: Law
additionRule = Law "Sum Rule" (Deriv (Var 'x') (BinOp Add (Var 'a') (Var 'b')), BinOp Add (Deriv (Var 'x') (Var 'a')) (Deriv (Var 'x') (Var 'b')))

productRule :: Law
productRule = Law "Product Rule" (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')), BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b'))))

quoRule :: Law
quoRule = Law "Quotient Rule" (Deriv (Var 'x') (BinOp Div (Var 'a') (Var 'b')), BinOp Div (BinOp Sub (BinOp Mul (Var 'b') (Deriv (Var 'x') (Var 'a'))) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b')))) (BinOp Pow (Var 'b') (Const 2)))

derivOfSelf :: Law
derivOfSelf = Law "Derivative of Self" (Deriv (Var 'x') (Var 'x'), Const 1)

mulByZero1 :: Law
mulByZero1 = Law "Multiplication by Zero" (BinOp Mul (Const 0) (Var 'x'), Const 0)

mulByZero2 :: Law
mulByZero2 = Law "Multiplication by Zero" (BinOp Mul (Var 'x') (Const 0), Const 0)

addZero1 :: Law
addZero1 = Law "Addition with Zero" (BinOp Add (Const 0) (Var 'x'), (Var 'x'))

addZero2 :: Law
addZero2 = Law "Addition with Zero" (BinOp Add (Var 'x') (Const 0), (Var 'x'))

mulByOne1 :: Law
mulByOne1 = Law "Multiplication by One" (BinOp Mul (Const 1) (Var 'x'), (Var 'x'))

mulByOne2 :: Law
mulByOne2 = Law "Multiplication by One" (BinOp Mul (Var 'x') (Const 1), (Var 'x'))

derivOfConst :: Law 
derivOfConst = Law "Derivative of Constant" (Deriv (Var 'x') (Var 'z'), Const 0)

derivSin :: Law
derivSin = Law "Derivative of Sin" (Deriv (Var 'x') (Unary Sin (Var 'a')), BinOp Mul (Unary Cos (Var 'a')) (Deriv (Var 'x') (Var 'a')))

derivCos :: Law
derivCos = Law "Derivative of Cos" (Deriv (Var 'x') (Unary Cos (Var 'a')), Unary Negation (Unary Sin (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'a')))))

derivLn :: Law
derivLn = Law "Derivative of Ln" (Deriv (Var 'x') (Unary Ln (Var 'a')), BinOp Mul (BinOp Div (Const 1) (Var 'a')) (Deriv (Var 'x') (Var 'a')))

powerRule :: Law
powerRule = Law "Power Rule" (Deriv (Var 'x') (BinOp Pow (Var 'a') (Var 'b')), BinOp Mul (BinOp Pow (Var 'a') (Var 'b')) (Deriv (Var 'x') (BinOp Mul (Var 'b') (Unary Ln (Var 'a')))))

laws :: [Law]
laws = [additionRule, productRule, quoRule, derivSin, derivCos, derivLn, powerRule, derivOfSelf, derivOfConst, mulByZero1, mulByZero2, mulByOne1, mulByOne2, addZero1, addZero2]