module Main where
import Data.List
import Data.Char
import System.Environment


main = do
    args <- getArgs
    let compFile = args !! 0
    let file = args !! 1
    let treeFile = args !! 2

    compContent <- readFile compFile
    tree <- readFile treeFile
    let content = huffmandecompress (read tree) compContent
    

    writeFile file (content)
    putStrLn "file decompressed"



-- I/O
-----------------------------------------------------------
-- Pure


data Tree  = Leaf Char Int
            | Branch Tree Tree Int
            deriving (Show, Read)


huffmandecompress :: Tree -> String -> String
huffmandecompress tree str = trace tree str
    where  trace (Leaf c _) []             = [c]
           trace (Leaf c _) code           = [c] ++ trace tree code
           trace (Branch l r _) (bit:code) = trace (if bit == '0' then l else r) code

