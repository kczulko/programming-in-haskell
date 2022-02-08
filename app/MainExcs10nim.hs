{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Data.Foldable (traverse_)
import Data.Char (isDigit, digitToInt)
import Text.Read (readEither)

data Player = One | Two deriving Show

next :: Player -> Player
next One = Two
next Two = One

type Board = [Int]

-- >>> initial
-- [5,4,3,2,1]
initial :: Board
initial = reverse [1..5]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !!  (row -1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard board = traverse_ (uncurry putRow) $ zip [1..] board

getDigit :: String -> IO Int
getDigit prompt = do putStrLn prompt
                     x <- getLine
                     case readEither x of                     
                       Right s -> return s
                       Left s -> do putStrLn "ERROR: Invalid digit"
                                    getDigit prompt

newline = putChar '\n'

play :: Board -> Player -> IO ()
play board player =
  do newline
     putBoard board
     if finished board then
       do newline
          putStr "Player "
          putStr (show (next player))
          putStrLn " wins!!!"
     else
       do newline
          putStr "Player "
          print player
          row <- getDigit "Enter a row number: "
          num <- getDigit "Stars to remove: "
          if valid board row num then
            play (move board row num) (next player)
          else
            do newline
               putStrLn "ERROR: Invalid move"
               play board player

main :: IO ()
main = play initial One

