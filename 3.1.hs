-- 12:26-12:46
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )  
import Data.Char (ord)
import Data.List (intersect)
import GHC.Unicode (isUpper)

main = do  
        handle <- openFile "in-3.txt" ReadMode
        contents <- hGetContents handle
        let allLines = lines contents
            allSacks = map (\s -> splitAt (length s `div` 2) s) allLines
        print $ foldr (\(compart1,compart2) b -> b + (getPriority . head $ intersect compart1 compart2)) 0 allSacks


getPriority :: Char -> Int
getPriority c 
    |isUpper c  = ord c - ord 'A' + 27
    |otherwise  = ord c - ord 'a' + 1