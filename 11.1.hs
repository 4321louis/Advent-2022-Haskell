--11:45-11:52 11:59-12:20 12:40-1:02 11:35-11:45 2:23-2:43
import Data.List.Split
import Data.List
import Debug.Trace

data Monkey = Monkey [Int] (Int->Int) (Int->Bool) Int Int Int

instance Show Monkey where
    show (Monkey ls _ _ t f _) = "Monkey:("++show ls ++","++ show t ++","++ show f++ ")"

main :: IO ()
main = do 
    content <- readFile "in-11.txt"
    let monkeylines = splitWhen (=="") . lines $ content 
        monkeys = map parseMonkey monkeylines
    print $ product . take 2 . reverse . sort . map (\(Monkey _ _ _ _ _ x)->x) $ iterate doRound monkeys !! 20


doRound :: [Monkey] -> [Monkey]
doRound monkeys = foldl doTurn monkeys [0..length monkeys - 1]

doTurn :: [Monkey] -> Int -> [Monkey] 
doTurn monkeys monkeyi = let
    (Monkey items worry test tMonkeyi fMonkeyi inspects) = monkeys !! monkeyi
    inspect = (`div`3) . worry
    inspectedItems = map inspect items
    itemsOf (Monkey items _ _ _ _ _) = items
    replaceItems items (Monkey _ w t tm fm i) = Monkey items w t tm fm i
    tMonkey = monkeys !! tMonkeyi
    fMonkey = monkeys !! fMonkeyi
    tMonkey0 = replaceItems (filter test inspectedItems ++ itemsOf tMonkey) tMonkey
    fMonkey0 = replaceItems (filter (not.test) inspectedItems ++ itemsOf fMonkey) fMonkey
    in replaceAt fMonkeyi fMonkey0 . replaceAt tMonkeyi tMonkey0 . replaceAt monkeyi (Monkey [] worry test tMonkeyi fMonkeyi (inspects+length items)) $ monkeys

parseMonkey :: [String] -> Monkey
parseMonkey ls = let
    items = map read . splitWhen (==',') . drop 18 $ ls !! 1
    [v1,o,v2] = drop 3 . words $ ls !! 2
    parse v = if v=="old" then id else const (read v)
    oper::Int->Int->Int = case o of "+"->(+); "*"->(*) 
    operation x =oper (parse v1 x) (parse v2 x)
    test = (==0) .  (flip rem . read . last . words $ ls !! 3)
    tmonkey = read . last . words $ ls !! 4
    fmonkey = read . last . words $ ls !! 5
    in Monkey items operation test tmonkey fmonkey 0

    
replaceAt :: Int -> a -> [a] -> [a]
replaceAt pos elem array = let (f,_:b) = splitAt pos array in f ++ elem:b