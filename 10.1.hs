--9:27-10:05
import Data.List.Split

main :: IO ()
main = do 
    content <- readFile "in-10.txt"
    let moves::[Int->Int] = concatMap ((\ws -> if head ws == "noop" then [(+0)] else [(+0),(+read (ws !! 1))]). words) $ lines content
        progressingState = 1:progressiveValues moves 1
    print . sum . map (uncurry (*)) . filter ((==0). flip rem 40 .(+ (-20)). snd) $ zip progressingState [1..]

progressiveValues :: [t -> t] -> t -> [t]
progressiveValues [] _ = []
progressiveValues (f:fs) a = f a: progressiveValues fs (f a)