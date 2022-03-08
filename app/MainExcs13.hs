module Main where

import Excs13
import Lib

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra    = "QCD \ESC\BS\DEL\n"

beep :: IO ()
beep = putStrLn "\BEL"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box ]

display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
               process c xs
             else
               do beep
                  calc xs

process :: Char -> String -> IO ()
process c xs | containsc "qQ\ESC" = quit
             | containsc "dD\BS\DEL" = delete xs
             | containsc "=\n" = eval xs
             | containsc "cC" = clear
             | otherwise = press c xs
             where
               containsc = elem c

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc . init $ xs

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, [])] -> calc (show n)
            _         -> beep *> calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

main = run
