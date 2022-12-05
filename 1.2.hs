import System.IO ( hGetContents, openFile, IOMode(ReadMode) )  
import Data.List ( sort )

main :: IO ()
main = do  
        handle <- openFile "in-1.txt" ReadMode
        contents <- hGetContents handle
        let allLines = lines contents
            allElves = foldr (\num out@(elf:elfs) -> if num == "" then []:out else (read num:elf):elfs) [[]] allLines
        print . sum . take 3 . reverse . sort $ map sum allElves
