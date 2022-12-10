-- 8:52-9:26
import Data.List

data GameState = GameState [(Int,Int)] [(Int,Int)]

main :: IO ()
main = do 
    content <- readFile "in-9.txt"
    let moves = concatMap parseMove $ lines content
        initial = GameState [] (replicate 10 (0,0))
        final = foldl doMove initial moves
        (GameState tailposes _) = final
    print . length . nub $ tailposes


doMove :: GameState -> (Int, Int) ->  GameState
doMove (GameState tailhistory knotposes) move = let 
    knotposes0 = add (head knotposes) move:tail knotposes
    knotposes1 = knotFollow knotposes0
    in GameState (last knotposes1:tailhistory) knotposes1

knotFollow :: [(Int,Int)] -> [(Int,Int)]
knotFollow [x] = [x]
knotFollow (head:(next:rest)) = let 
    xdif = (fst head - fst next)
    ydif = (snd head - snd next)
    next0 = if abs xdif >= 2 || abs ydif >= 2 then add next (signum xdif,signum ydif) else next
    in head:knotFollow (next0:rest)


parseMove :: [Char] -> [(Int, Int)]
parseMove (dir:list) = let x = read (tail list) in replicate x $ dirOf dir

dirOf :: Char -> (Int, Int)
dirOf 'U' = (0,1)
dirOf 'D' = (0,-1)
dirOf 'R' = (1,0)
dirOf 'L' = (-1,0)


add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (x,y) (x2,y2) = (x+x2,y+y2)

