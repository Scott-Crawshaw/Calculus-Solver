module Main where

import Parse
import Calculate
import Laws
import DataStructures

main :: IO ()
main = undefined

--The below code not work
{-main = do
    putStrLn "Enter an expression to derive"
    input <- getLine
    inputExp <- (parseInputExpr input)
    isInputCorrect <- (isCorrect inputExp)
    if not isInputCorrect
        then do 
            print inputExp
        else do
            expr <- (extractExpr inputExp)
            derivation <- (derive laws expr)
            print derivation
    
isCorrect :: Result a -> Bool
isCorrect (Correct a) = True
isCorrect _ = False

extractExpr :: Result Expr -> Expr
extractExpr (Correct e) = e
extractExpr _ = undefined-}