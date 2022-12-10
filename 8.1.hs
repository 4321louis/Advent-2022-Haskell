--4:37-5:09
import Data.List
main :: IO ()
main = do 
    content <- readFile "in-8.txt"
    let trees = map (map (read . pure)) $ lines content
        coords = [(x-1,y-1)|x <- [1..length trees],y<-[1..length trees]]
        visibleTrees = filter (isVisible trees) coords
    print $ length visibleTrees

isVisible :: [[Int]] -> (Int,Int) -> Bool
isVisible rows (x,y) =  
        let cols = transpose rows
            up = reverse $ take (y+1) (cols !! x)
            left = reverse $ take (x+1) (rows!! y)
            right = drop x (rows !! y) 
            down = drop y (cols !! x) 
            in any (\dir -> all (<head dir) (tail dir)) [up,left,down,right]