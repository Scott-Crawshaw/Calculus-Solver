-- File reading code drawn from https://stackoverflow.com/a/7867786
-- REPL code drawn from https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2

module Main where

import Parse
import Calculate
import DataStructures
import System.IO
import Control.Monad (unless)

main :: IO ()
main = do
    input <- readLine
    handle <- openFile "LawList.txt" ReadMode
    contents <- hGetContents handle
    let singlelines = lines contents
        list = mapLaws singlelines
    unless(input == ":quit")
        $ printCalc (derive list (parseInputExpr input)) >> main

mapLaws :: [String] -> [Law]
mapLaws = concat . (map parseInputLaw)

readLine :: IO String
readLine = putStr "Enter an expression to parse\n"
    >> hFlush stdout
    >> getLine

printCalc :: Result Calculation -> IO()
printCalc calc = putStrLn (show calc)