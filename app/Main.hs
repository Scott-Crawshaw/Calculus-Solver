module Main where

import Parse

main :: IO ()
main = do
    putStrLn "Enter an expression to parse"
    input <- getLine
    print$ parseInputExpr input
