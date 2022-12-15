{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
--11:01-12:44 9:36-9:47 +~15 mins
import Data.List.Split ( splitWhen )
import Debug.Trace

data WeirdList = List [WeirdList] | Only Int

main :: IO ()
main = do 
    content <- readFile "in-13.txt"
    print $ sum . map fst. filter (correctOrder.snd) . zip [1..] . splitWhen (=="") $ lines content


correctOrder :: [[Char]] -> Bool
correctOrder [f,s] = let 
    (_,(r1@(h1:t1),r2@(h2:t2))) = findIdenticalPrefix f s 
    in
        if h1 == ']' then True else
        if h2 == ']' then False else (
        let [n1,n2] = map (takeWhile (`notElem` ",]")) [r1,r2]
            [nr1,nr2] = map (dropWhile (`notElem` ",]")) [r1,r2]
        in 
            if h1 == '[' then correctOrder [r1, concat ["[",n2,"]",nr2]] else
            if h2 == '[' then correctOrder [concat ["[",n1,"]",nr1], r2] else 
            if null n1 then True else
            if null n2 then False else
            (read n1::Int)<read n2)
            



findIdenticalPrefix :: Eq a => [a] -> [a] -> ([a], ([a], [a]))
findIdenticalPrefix [] [] = ([],([],[])) 
findIdenticalPrefix [] s2 = ([],([],s2)) 
findIdenticalPrefix s1 [] = ([],(s1,[])) 
findIdenticalPrefix s1@(h1:t1) s2@(h2:t2)
    |  h1 /= h2 = ([],(s1,s2))
    | otherwise = let (pre,tails) = findIdenticalPrefix t1 t2 in (h1:pre,tails)