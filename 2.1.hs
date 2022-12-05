import System.IO ( hGetContents, openFile, IOMode(ReadMode) )

main :: IO ()
main = do  
        handle <- openFile "in-2.txt" ReadMode
        contents <- hGetContents handle
        let allLines = lines contents
            roundsScores = map (\line -> let [o, m]= words line in score (toPlay m) (toPlay o)) allLines
        print $ sum roundsScores

score :: Play -> Play -> Integer
score me opponent = shapeScore me + outcomeScore opponent me 
    where
        shapeScore Rock = 1
        shapeScore Paper = 2
        shapeScore Scissors = 3
        outcomeScore o m
            | o == m = 3
            | beats m == o = 0
            | otherwise = 6


beats :: Play -> Play
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

data Play = Rock | Paper | Scissors deriving (Eq)

toPlay :: String -> Play
toPlay "A" = Rock
toPlay "B" = Paper
toPlay "C" = Scissors
toPlay "X" = Rock
toPlay "Y" = Paper
toPlay "Z" = Scissors