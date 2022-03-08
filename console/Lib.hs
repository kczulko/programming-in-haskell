module Lib where

import System.IO ( hSetEcho, stdin ) 

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)
type Board = [Pos]

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x


goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
