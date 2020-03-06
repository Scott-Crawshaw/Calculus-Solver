import Parse
import Calculate
import DataStructures
import System.IO
import Control.Monad (unless)

getLaws :: [Law]
getLaws = do
    handle <- openFile "LawList.txt" ReadMode
    contents <- hGetContents handle
    let singlelines = lines contents
        list = mapLaws singlelines
    list

mapLaws :: [String] -> [Law]
mapLaws = concat . (map parseInputLaw)