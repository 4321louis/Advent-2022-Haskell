--11:47-12:03 forgor to time the rest F
import Data.List.Split
import Data.List
import Debug.Trace
import Data.Char

main :: IO ()
main = do 
    content <- readFile "in-12.txt"
    let ls = lines content
        rows = map (map (\x->
            case x of
                'S'->ord 'a'
                'E'->ord 'z'
                _->ord x)) ls
    print $ fst . head . dropWhile (\(i,(_,coords))-> findCoords ls 'E' `notElem` coords) . zip [0..] $ iterate (findAdjacents rows) ([],[findCoords ls 'S'])



findCoords ::Eq a =>[[a]] -> a -> (Int,Int)
findCoords rows e= let 
    Just y = findIndex (elem e) rows 
    Just x = elemIndex e (rows !! y)
    in (x,y)

findAdjacents :: [[Int]]-> ([(Int,Int)],[(Int,Int)]) -> ([(Int,Int)],[(Int,Int)])
findAdjacents rows (seen,targets) = let targets0 =nub .filter (`notElem` seen). concatMap (\(x,y)-> filter (moveable rows (x,y)) [(x - 1, y),(x + 1, y),(x, y+1),(x, y-1)]) $ targets in 
    (seen ++ targets,targets0)

moveable ::[[Int]] -> (Int, Int) -> (Int, Int) -> Bool
moveable rows (x1,y1) (x2,y2) = 
    and ([(>=0),(<length (head rows))] <*> [x1,x2]) && 
    and ([(>=0),(<length rows)] <*> [y1,y2]) &&
    rows!!y2!!x2 - rows!!y1!!x1 <=1  
    
