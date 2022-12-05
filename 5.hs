--10:42-1:12(with much procrastination and wonderiing why foldr wasn't folding the right way)
import Data.List
import Data.List.Split
main :: IO ()
-- main = readFile "in-5.txt" >>= print . map sum . transpose . map ((\[l1,h1,l2,h2]-> [fromEnum (l1<=l2&&h2<=h1||l2<=l1&&h1<=h2),fromEnum (l1<=h2&&h2<=h1||l2<=h1&&h1<=h2)]) . map (read::String->Int) . splitWhen (`elem` ",-")) . lines

main = do 
    content <- readFile "in-5.txt"
    let [inBoxes,inMoves] = splitWhen (=="") $ lines content
        boxes = map (reverse . filter (/=' ')) . transpose . map (foldr (\(mark,val) b-> if mark =='.' then val:b else b) [] . zip (cycle "[.] ")) . tail . reverse $ inBoxes
        moves =  take 199999 $ map (\line -> let toInt = (read ::String->Int);[_,amount,_,from,_,to] = splitWhen (==' ') line in (toInt amount,(-1) + toInt from,(-1) + toInt to)) inMoves
    print boxes
    print moves
    let movedBoxes2 = foldl (\boxes2 (a,f,t) -> let 
            toBoxes = boxes2 !! t
            (movedBoxes,rest) = splitAt a (boxes2 !! f) 
            in replaceAt f rest . replaceAt t (movedBoxes++toBoxes) $ boxes2) boxes moves
    print movedBoxes2
    print $ map head movedBoxes2

replaceAt pos elem array = let (f,_:b) = splitAt pos array in f ++ elem:b