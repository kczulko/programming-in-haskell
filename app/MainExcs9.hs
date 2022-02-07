module Main where
import Excs9 (solutions')

main :: IO ()
main = putStrLn $ show (solutions' [1,3,7,10,25,50] 765)
