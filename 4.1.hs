-- 6:02-6:26
import System.IO ( hGetContents, openFile, IOMode(ReadMode) ) 
import Data.List

main = do  
        handle <- openFile "in-4.txt" ReadMode
        contents <- hGetContents handle
        let allLines = lines contents
            allPairs = map (\str -> let 
                (e1,_:e2) = split ',' str 
                (e1f,_:e1b) = split '-' e1 
                (e2f,_:e2b) = split '-' e2
                in ((read e1f::Int,read e1b::Int),(read e2f::Int,read e2b::Int))) allLines

        print $ foldr (\((e1f,e1b),(e2f,e2b)) b -> b + if e1f<=e2f && e1b>=e2b || e1f>=e2f && e1b<=e2b then 1 else 0) 0 allPairs

split :: Eq a => a -> [a] -> ([a], [a])
split c s = splitAt (head $ elemIndices c s) s

