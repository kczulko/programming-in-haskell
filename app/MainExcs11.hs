{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Char
import Data.List
import System.IO
import System.Random (randomRIO)

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

data Tree a = Node a [Tree a] deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g = []
          | full g = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) | wins O g = Node (g,O) []
                    | wins X g = Node (g,X) []
                    | otherwise = Node (g,B) []
minimax (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                    | turn g == X = Node (g, maximum ps) ts'
                      where ts' = map minimax ts
                            ps = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax tree

bestmove2 :: Grid -> Player -> IO Grid
bestmove2 g p =
    do let tree = prune depth (gametree g p)
       let Node (_,best) ts = minimax tree
       let bmoves = [g' | Node (g',p') _ <- ts, p' == best]
       bmove <- randomRIO (0, length bmoves - 1)
       return (bmoves !! bmove)

bestmove3 :: Grid -> Player -> Grid
bestmove3 g p = fst (last shallowFirst)
  where
    shallowFirst = sortOn snd candidates
    candidates = [(g', countdepth n) | n @ (Node (g',p') _) <- ts, p' == best]
    tree = prune depth (gametree g p)
    Node (_,best) ts = minimax tree

-- main :: IO ()
-- main = tictactoe

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p | wins O g = putStrLn "Player O wins!\n"
          | wins X g = putStrLn "Player X wins!\n"
          | full g   = putStrLn "It's a draw!\n"
          | p == O = do i <- getNat (prompt p)
                        case move g i p of
                          [] -> do putStrLn "Error: Invalid move"
                                   play' g p
                          g':_ -> play g' (next p)
          | p == X = do putStrLn "Player X is thinking..."
                        -- let bmove = bestmove g p
                        (play $! bestmove g p) (next p)

-- >>> countnodes (gametree empty O)
-- 549946
countnodes :: Tree a -> Int
countnodes (Node a []) = 1
countnodes (Node a (n:ns)) = 1 + countnodes n + sum [countnodes n' | n' <- ns]

-- >>> countdepth (gametree empty O)
-- 9
countdepth :: Tree a -> Int
countdepth (Node a []) = 0
countdepth (Node a ns) = 1 + maximum (map countdepth ns)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Do you want to start? (type 'yes' or 'no')"
          str <- getLine
          case str of
            "yes" -> play empty O
            "no"  -> play empty X
            _     -> main
