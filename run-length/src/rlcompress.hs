module Main where
import Data.List
import System.Environment

main = do
    args <- getArgs
    let fileToRead = args !! 0
    let fileToWrite = args !! 1
    
    content <- readFile fileToRead
    let compContent = rlcompress content
    
    let len     = length content
    let compLen = length compContent
    let factor  = fromIntegral (round ((fromIntegral compLen) / (fromIntegral len) * 1000)) / 10
    
    writeFile fileToWrite (compContent)

    putStrLn ("length of " ++ fileToRead ++ ": " ++ (show len) ++ " characters")
    putStrLn ("length of compressed file " ++ (show compLen) ++ ": " ++ " characters")
    putStrLn ("factor: " ++ (show factor) ++ "%")



-- I/O
-----------------------------------------------------------
-- Pure


rlcompress :: String -> String
rlcompress str = concat . map (\x -> (sizeStr x) ++ [head x]) . group $ str 
    where sizeStr s
            | length s > 1 = show . length $ s
            | otherwise     = ""
