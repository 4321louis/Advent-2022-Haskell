import System.IO ( hGetContents, openFile, IOMode(ReadMode) )  

main = do  
        handle <- openFile "in-1.txt" ReadMode
        contents <- hGetContents handle
        let allLines = lines contents
            allElves = foldr (\num out@(elf:elfs) -> if num == "" then []:out else (read num:elf):elfs) [[]] allLines
        print $ maximum $ map sum allElves
