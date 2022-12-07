-- 9:26-11:00
import Data.List
import Data.Maybe
import Debug.Trace
data Icon = Folder String Directory | File Int deriving (Show)
type Directory = [Icon]

main :: IO ()
main = do 
    content <- readFile "in-7.txt"
    let (_,system) = foldl (\(path,full@(Folder s i)) line -> let w = words line in if head w =="$" 
        then if w !! 1 == "ls" then (path,full) else case w !! 2 of ".."->(tail path,full);"/"-> ([],full);_->(w !! 2:path,full) 
        else if head w == "dir" then (path,insertAt full (reverse path) (Folder (w!!1) []) ) else (path,insertAt full (reverse path) (File (read (head w))))) ([],Folder "/" []) $ lines content
    print $ sum . filter (<=100000) . map sizeOf $ flatten system

sizeOf :: Icon -> Int
sizeOf (File i) = i
sizeOf (Folder _ i) = foldr ((+).sizeOf) 0 i

flatten :: Icon -> [Icon]
flatten (File _) = []
flatten this@(Folder s i) = this:concatMap flatten i

insertAt :: Icon -> [String] -> Icon -> Icon
insertAt (Folder s icons) [] e = Folder s (e:icons)
insertAt (Folder s icons) (x:xs) e = let 
    isPath (Folder s _) = s==x
    isPath _ = False
    nextF = fromJust $ find isPath icons in Folder s (insertAt nextF xs e:filter (not . isPath) icons)