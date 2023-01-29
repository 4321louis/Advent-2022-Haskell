--11:47-12:03 forgor to time the rest F
import Data.List.Split
import Data.List
import Debug.Trace
import Data.Char
import Data.Graph
import Data.Graph.AStar
import qualified Data.HashSet as HS
import Data.Maybe
import qualified Data.Map as M

main :: IO ()
main = do 
    content <- readFile "in-12.txt"
    let ls = lines content
        rows = map (map (\x->
            case x of
                'S'->ord 'a'
                'E'->ord 'z'
                _->ord x)) ls
        exit = findCoords ls 'E'
        graph = generateGraph rows
        solutioner goal@(gx,gy) = aStar 
            (fromJust . (`M.lookup` graph))
            (\(x1,y1) (x2,y2) -> (x1-x2)^2 +  (y1-y2)^2)
            (\(x,y) -> (gx-x)^2 +  (gy-y)^2)
            (==goal)
        solution = solutioner exit (findCoords ls 'S')
        solution2 = solutioner (findCoords ls 'a') exit
        solution3 = solutioner (findCoords ls 'c') exit
        solution4 = solutioner (findCoords ls 'b') exit
        solution5 = solutioner (findCoords ls 'e') exit
        solution6 = solutioner (findCoords ls 'f') exit
        solution7 = solutioner (findCoords ls 'g') exit
            

    print . length . fromJust $ solution
    print . length . fromJust $ solution2 
    print . length . fromJust $ solution3 
    print . length . fromJust $ solution4 
    print . length . fromJust $ solution5 
    print . length . fromJust $ solution6 
    print . length . fromJust $ solution7    
    -- print $ fst . head . dropWhile (\(i,(_,coords))-> trace (show i ++ show coords) (findCoords ls 'E' `notElem` coords)) . zip [0..] $ iterate (findAdjacents rows) ([],[findCoords ls 'S'])


generateGraph :: [[Int]] -> M.Map (Int,Int) (HS.HashSet (Int,Int))
generateGraph rows = let coords = findAllCoords rows
    in M.fromList $ map (\n@(x,y)-> (n,HS.fromList (findAdjacent rows n))) coords


findAllCoords ::Eq a =>[[a]] -> [(Int,Int)]
findAllCoords rows = [(x,y)|x<-[0..length (head rows) -1],y<-[0..length rows -1]]



findCoords ::Eq a =>[[a]] -> a -> (Int,Int)
findCoords rows e= let 
    Just y = findIndex (elem e) rows 
    Just x = elemIndex e (rows !! y)
    in (x,y)

findAdjacent :: [[Int]]-> (Int,Int) -> [(Int,Int)]
findAdjacent rows target@(x,y) = filter (moveable rows target) [(x - 1, y),(x + 1, y),(x, y+1),(x, y-1)]

moveable ::[[Int]] -> (Int, Int) -> (Int, Int) -> Bool
moveable rows (x1,y1) (x2,y2) = 
    and ([(>=0),(<length (head rows))] <*> [x1,x2]) && 
    and ([(>=0),(<length rows)] <*> [y1,y2]) &&
    rows!!y2!!x2 - rows!!y1!!x1 <=1  
    
