module TestHelper where

import Parse
import DataStructures
import Calculate

-- Helper functions for Spec.hs

lawStrings :: [String]
lawStrings = ["Cancellation Rule: (a - a) = 0","Addition Rule: (deriv x (a + b)) = (deriv x a) + (deriv x b)","Subtraction Rule: (deriv x (a - b)) = (deriv x a) - (deriv x b)","Product Rule: (deriv x (a * b)) = (deriv x a) * b + a * (deriv x b)","Quotient Rule: (deriv x (a / b)) = ((b * (deriv x a)) - (a * (deriv x b))) / (b^2)","Power Rule: deriv x (a ^ b) = a ^ b * (deriv x (b * ln(a)))","Derivative of Sin: deriv x (sin a) = (cos a) * (deriv x a)","Derivative of Cos: deriv x (cos a) = -(sin a) * (deriv x a)","Derivative of Ln: deriv x (ln a) = (1 / a) * (deriv x a)","Derivative of Self: deriv x x = 1","Derivative of Constant: deriv x z = 0","Derivative of Fixed Variable: deriv x q = 0","Derivative of Negation: deriv x -a = -(deriv x a)","Multiplication by Zero: 0 * x = 0","Multiplication by Zero: x * 0 = 0","Multiplication by One: 1 * x = x","Multiplication by One: x * 1 = x","Addition with Zero: 0 + x = x","Addition with Zero: x + 0 = x","Subtraction with Zero: 0 - x = x","Subtraction with Zero: x - 0 = x", "Zero is Non-Negative: -0 = 0","Rewrite Fraction as Exponent: a / x = a * (x ^ (-1))","Rewrite Fraction as Exponent: a / (x ^ b) = a * (x ^ (-b))","Combine Exponents: (x ^ a) * (x ^ b) = (x ^ (a + b))","Combine Exponents: (x ^ a) * (z * (x ^ b)) = z * (x ^ (a + b))","Add and Subtract: x + -(z) = x - z","Simplify Exponent: x ^ 1 = x"]

laws :: [Law]
laws = concat (map parseInputLaw lawStrings)

parseExprTest :: Bool
parseExprTest = show (parseInputExpr "deriv x x^2 + 3*x - 4 + sin(5*cos(6))") == "deriv x ((((x^2) + (3 * x)) - 4) + sin ((5 * cos (6))))"

deriveFuncTest :: Bool
deriveFuncTest = (derivation!!((length derivation) - 2)) == '0'

derivation :: String
derivation = show (derive laws (parseInputExpr "deriv x x^2 - 4 - x^2"))