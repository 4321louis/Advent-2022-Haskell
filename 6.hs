-- 5:47-6:03
import Data.List;

main = readFile "in-6.txt" >>= let solve s l@(x:xs) = if (s==) . length . nub $ take s l then s else 1 + solve s xs in print . (\c ->(solve 4 c,solve 14 c))
