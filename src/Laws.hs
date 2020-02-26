module Laws (laws) where
import DataStructures
import Parse

--additionRule :: Law
--additionRule = Law "Sum Rule" (Deriv (Var 'x') (BinOp Add (Var 'a') (Var 'b')), BinOp Add (Deriv (Var 'x') (Var 'a')) (Deriv (Var 'x') (Var 'b')))

--productRule :: Law
--productRule = Law "Product Rule" (Deriv (Var 'x') (BinOp Mul (Var 'a') (Var 'b')), BinOp Add (BinOp Mul (Deriv (Var 'x') (Var 'a')) (Var 'b')) (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'b'))))

derivSin :: Law
derivSin = Law "Derivative of Sin" (Deriv (Var 'x') (Unary Sin (Var 'a')), BinOp Mul (Unary Cos (Var 'a')) (Deriv (Var 'x') (Var 'a')))

derivCos :: Law
derivCos = Law "Derivative of Cos" (Deriv (Var 'x') (Unary Cos (Var 'a')), Unary Negation (Unary Sin (BinOp Mul (Var 'a') (Deriv (Var 'x') (Var 'a')))))

derivLn :: Law
derivLn = Law "Derivative of Ln" (Deriv (Var 'x') (Unary Ln (Var 'a')), BinOp Mul (BinOp Div (Const 1) (Var 'a')) (Deriv (Var 'x') (Var 'a')))

powerRule :: Law
powerRule = Law "Power Rule" (Deriv (Var 'x') (BinOp Pow (Var 'a') (Var 'b')), BinOp Mul (BinOp Pow (Var 'a') (Var 'b')) (Deriv (Var 'x') (BinOp Mul (Var 'b') (Unary Ln (Var 'a')))))

laws :: [Law]
laws = [derivSin, derivCos, derivLn, powerRule]