module Main where
import Data.List
import Data.Char
import System.Environment


main = do
    args <- getArgs
    let fileToRead = args !! 0
    let fileToWrite = args !! 1
    
    compContent <- readFile fileToRead
    let content = rldecompress compContent
    
    writeFile fileToWrite (content)
    putStrLn "file decompressed"


-- I/O
-----------------------------------------------------------
-- Pure



readInt :: String -> Int
readInt x = read (takeWhile isDigit x) :: Int


split :: String -> [String]
split str
    | length str > 0 = [(takeWhile (isDigit) str) ++ [head $ dropWhile (isDigit) str] ] ++ split (tail $ dropWhile (isDigit) str)
    | otherwise      = []


rldecompress :: String -> String
rldecompress str = concatMap (\x -> take (takeInt x) (repeat (last x)) ) (split str) 
    where takeInt y
            | isDigit (head y) = readInt (init y)
            | otherwise        = 1




        



    
    





