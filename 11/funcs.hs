import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]
data Player = o | b | x
              deriving (Eq, Ord, Show)

next :: Player -> Player
next o = x
next b = b
next x = o

empty :: Grid
empty = replicate size (replicate size b)

full :: Grid -> Bool
full = all (/= b) . concat

turn :: Grid -> Player
turn g = if os <= xs then o else x
         where
           os = length (filter (== o) ps)
           xs = length (filter (== x) ps)
           ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
             line = all (== p)
             rows = g
             cols = transpose g
             dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins o g || wins x g

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer o = ["   ", " o ", "   "]
showPlayer b = ["   ", "   ", "   "]
showPlayer x = ["   ", " x ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y   ] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == b

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, b:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

