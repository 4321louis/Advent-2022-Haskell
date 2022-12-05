-- 12:47-1:00
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )  
import Data.Char (ord)
import Data.List (intersect)
import GHC.Unicode (isUpper)

main = do  
        handle <- openFile "in-3.txt" ReadMode
        contents <- hGetContents handle
        let allLines = lines contents
            allSacks = divideEvenly allLines 3
            -- allGroups = allLines
        print $ foldr (\[sack1,sack2,sack3] b -> b + (getPriority . head . intersect sack3 $ intersect sack1 sack2)) 0 allSacks

divideEvenly :: [a] -> Int -> [[a]]
divideEvenly [] _ = []
divideEvenly list size = front:divideEvenly back size where (front,back) = splitAt size list

getPriority :: Char -> Int
getPriority c 
    |isUpper c  = ord c - ord 'A' + 27
    |otherwise  = ord c - ord 'a' + 1