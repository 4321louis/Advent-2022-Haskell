-- 8:09-8:51
import Data.List

data GameState = GameState [(Int,Int)] (Int,Int) (Int,Int)

main :: IO ()
main = do 
    content <- readFile "in-9.txt"
    let moves = concatMap parseMove $ lines content
        initial = GameState [] (0,0) (0,0)
        final = foldl doMove initial moves
        (GameState tailposes _ _) = final
    print . length . nub $ tailposes


doMove :: GameState -> (Int, Int) ->  GameState
doMove (GameState tailhistory headpos tailpos) move = let 
    headpos0 = add headpos move 
    tailpos0 = if abs (fst tailpos - fst headpos0) >= 2 || abs (snd tailpos - snd headpos0) >= 2 then headpos else tailpos
    in GameState (tailpos0:tailhistory) headpos0 tailpos0


parseMove :: [Char] -> [(Int, Int)]
parseMove (dir:list) = let x = read (tail list) in replicate x $ dirOf dir

dirOf :: Char -> (Int, Int)
dirOf 'U' = (0,1)
dirOf 'D' = (0,-1)
dirOf 'R' = (1,0)
dirOf 'L' = (-1,0)


add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (x,y) (x2,y2) = (x+x2,y+y2)

