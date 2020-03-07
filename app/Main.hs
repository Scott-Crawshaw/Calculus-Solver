-- File reading code drawn from https://stackoverflow.com/a/7867786
-- REPL code drawn from https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2

module Main where

import Parse
import Calculate
import DataStructures
import System.IO
import Control.Monad (unless)

-- Exectuable interface
main :: IO ()
main = do
    input <- readLine
    handle <- openFile "LawList.txt" ReadMode
    contents <- hGetContents handle
    let singlelines = lines contents
        list = mapLaws singlelines
    unless(input == ":quit")
        $ printCalc (derive list (parseInputExpr input)) >> main

-- Takes a list of strings and converts each string into a Lawe
mapLaws :: [String] -> [Law]
mapLaws = concat . (map parseInputLaw)

-- Prompts user for input and returns input
readLine :: IO String
readLine = putStr "Enter an expression to parse\n"
    >> hFlush stdout
    >> getLine

-- Prints Calculation
printCalc :: Result Calculation -> IO()
printCalc calc = putStrLn (show calc)