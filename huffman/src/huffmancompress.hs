module Main where
import Data.List
import qualified Data.Map as DM
import Data.Function
import System.Environment


main = do
    args <- getArgs
    let file = args !! 0
    let compFile = args !! 1
    let treeFile =  args !! 2

    putStrLn compFile

    putStrLn treeFile
    
    content <- readFile file

    let tree = makeTree content
    let compContent = huffmancompress (makeCodeTable tree) content

    let len      = length content
    let compLen  = length compContent
    let factor   = fromIntegral (round ((fromIntegral compLen) / (fromIntegral (len*8)) * 1000)) / 10

    writeFile compFile compContent
    writeFile treeFile (show tree)

    putStrLn ("length of " ++ file ++ ": " ++ (show len) ++ " bytes")
    putStrLn ("length of compressed file: " ++ (show compLen) ++ " " ++ " bits")
    putStrLn ("factor: " ++ (show factor) ++ "%")



-- I/O
-----------------------------------------------------------
-- Pure


type Map = DM.Map
type CodeTable = Map Char [Char] 

data Tree  = Leaf Char Int
            | Branch Tree Tree Int
            deriving (Show, Read)



freq :: Tree -> Int
freq (Leaf _ f)     = f
freq (Branch _ _ f) = f



huffmancompress :: (Foldable t, Ord k) => Map k [b] -> t k -> [b]
huffmancompress m str = concatMap (m DM.!) str


countchars :: String -> [(Char, Int)]
countchars str = sortBy (compare `on` snd) $ map (\x -> (head x, length x)) $ group . sort $ str


makeTree :: String -> Tree
makeTree str = grow $ map (uncurry Leaf) charcount
    where grow ([t])      = t
          grow (a:b:list) = grow $ insertBy (compare `on` freq) (Branch a b (freq a + freq b)) list
           
          charcount = countchars $ str

           
makeCodeTable :: Tree -> CodeTable
makeCodeTable tree = DM.fromList . traverse $ tree
    where   traverse (Leaf c f)      = [(c, [])]
            traverse (Branch l r _)  = map (addBit '0') (traverse l) ++ map (addBit '1') (traverse r)
             where addBit x (a, b) = (a, [x] ++ b)
