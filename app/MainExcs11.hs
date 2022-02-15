module Main where

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
           os = length (filter (== O) ps)
           xs = length (filter (== X) ps)
           ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ diags)
           where
             line = all (==p)
             rows = g
             cols = transpose g
             diags = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer X = ["   ", " X ", "   "]
showPlayer B = ["   ", "   ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  [chop size (xs ++ [p] ++ ys) | valid g i]
  where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)



getNat :: String -> IO Int
getNat prompt = do _ <- putStrLn prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

type Pos = (Int,Int)

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!"
         | wins X g = putStrLn "Player X wins!"
         | full g   = putStrLn "It's a draw!"
         | otherwise =
           do i <- getNat (prompt p)
              case move g i p of
                [] -> do putStrLn "ERROR: Invalid move"
                         run' g p
                g':_ -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

tictactoe :: IO ()
tictactoe = run empty O

main :: IO ()
main = tictactoe
