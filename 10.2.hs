{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
--10:05-10:19 12:42-12:59 (31)
import Data.List.Split
import Data.List

main :: IO ()
main = do 
    content <- readFile "in-10.txt"
    let moves::[Int->Int] = concatMap ((\ws -> if head ws == "noop" then [(+0)] else [(+0),(+read (ws !! 1))]). words) $ lines content
        progressingState = 1:progressiveValues moves 1
    putStr . intercalate "\n" . chunksOf 40 . map (\(pos,cycle)->if abs(pos+1-rem cycle 40)<=1 then '#' else '.') $ zip progressingState [1..]

progressiveValues :: [t -> t] -> t -> [t]
progressiveValues [] _ = []
progressiveValues (f:fs) a = f a: progressiveValues fs (f a)
