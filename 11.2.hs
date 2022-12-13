--2:53-3:17
import Data.List.Split
import Data.List
import Debug.Trace

data Monkey = Monkey [Integer] (Integer->Integer) (Integer->Bool) Int Int Int

instance Show Monkey where
    show (Monkey ls _ _ t f _) = "Monkey:("++show ls ++","++ show t ++","++ show f++ ")"

main :: IO ()
main = do 
    content <- readFile "in-11.txt"
    let monkeylines = splitWhen (=="") . lines $ content 
        monkeys = map parseMonkey monkeylines
        maxWorry = toInteger . product . map extractMagicValue $ monkeylines
    print $ product . take 2 . reverse . sort . map (\(Monkey _ _ _ _ _ x)->x) $ iterate (doRound maxWorry) monkeys !! 10000


doRound :: Integer ->[Monkey] -> [Monkey]
doRound maxWorry monkeys = foldl (doTurn maxWorry) monkeys [0..length monkeys - 1]

doTurn :: Integer->[Monkey] -> Int -> [Monkey] 
doTurn maxWorry monkeys monkeyi = let
    (Monkey items worry test tMonkeyi fMonkeyi inspects) = monkeys !! monkeyi
    inspect = (`rem` maxWorry) . worry
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
    oper::Integer->Integer->Integer = case o of "+"->(+); "*"->(*) 
    operation x =oper (parse v1 x) (parse v2 x)
    test = (==0) .  (flip rem . read . last . words $ ls !! 3)
    tmonkey = read . last . words $ ls !! 4
    fmonkey = read . last . words $ ls !! 5
    in Monkey items operation test tmonkey fmonkey 0

extractMagicValue :: [String] -> Int
extractMagicValue ls = read . last . words $ ls !! 3


    
replaceAt :: Int -> a -> [a] -> [a]
replaceAt pos elem array = let (f,_:b) = splitAt pos array in f ++ elem:b