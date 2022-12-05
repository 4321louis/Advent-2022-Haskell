-- 6:27-6:34
import System.IO ( hGetContents, openFile, IOMode(ReadMode) ) 
import Data.List.Split hiding (split)
import Data.List (transpose,elemIndices)
import GHC.Base hiding (foldr)  
import Data.Maybe (fromJust)

main2 :: IO ()
main2 = do  
        contents <- readFile "in-4.txt"
        let allLines = lines contents
            allPairs = map (\str -> let 
                (e1,_:e2) = split ',' str 
                (e1f,_:e1b) = split '-' e1 
                (e2f,_:e2b) = split '-' e2
                in ((read e1f::Int,read e1b::Int),(read e2f::Int,read e2b::Int))) allLines

        print $ foldr (\((e1f,e1b),(e2f,e2b)) b -> b + if (e1f>=e2f && e1f<=e2b) || (e1b>=e2f && e1b<=e2b)|| (e1f<=e2f && e1b>=e2b) then 1 else 0) 0 allPairs

split :: Eq a => a -> [a] -> ([a], [a])
split c s = splitAt (head $ elemIndices c s) s


-- -- main :: IO ()
-- main22 = readFile "in-4.txt" >>= print . foldr ((\(l1,h1,l2,h2) [b1,b2]-> [b1 + fromEnum (l1<=l2&&h2<=h1||l2<=l1&&h1<=h2),b2 + fromEnum (l1>=h2&&h2<=h1||h1<=l1&&l1<=h2)]) . (\str -> let 
--                 (e1,_:e2) = split ',' str 
--                 (e1f,_:e1b) = split '-' e1 
--                 (e2f,_:e2b) = split '-' e2
--                 in (read e1f::Int,read e1b::Int,read e2f::Int,read e2b::Int))) [0,0] . lines

-- mainOneLine :: IO ()
-- mainOneLine = readFile "in-4.txt" >>= print . map sum . transpose . map ((\[l1,h1,l2,h2]-> [fromEnum (l1<=l2&&h2<=h1||l2<=l1&&h1<=h2),fromEnum (l1<=h2&&h2<=h1||l2<=h1&&h1<=h2)]) . map (read::String->Int) . splitWhen (`elem` ",-")) . lines
