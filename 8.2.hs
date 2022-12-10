--5:09-5:30
import Data.List
main :: IO ()
main = do 
    content <- readFile "in-8.txt"
    let trees = map (map (read . pure)) $ lines content
        coords = [(x-1,y-1)|x <- [1..length trees],y<-[1..length trees]]
        treeScores = map (scoreTree trees) coords
    print $ maximum treeScores

scoreTree :: [[Int]] -> (Int,Int) -> Int
scoreTree rows (x,y) = 
    let cols = transpose rows
        up = reverse $ take (y+1) (cols !! x)
        left = reverse $ take (x+1) (rows!! y)
        right = drop x (rows !! y) 
        down = drop y (cols !! x) in
            product (map (\dir -> let shorter = length $ takeWhile (< head dir) (tail dir) in shorter + if shorter+1 < length dir then 1 else 0) [up,left,right,down])