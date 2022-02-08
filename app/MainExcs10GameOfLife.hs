module Main where

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10
height :: Int
height = 10

main :: IO ()
main = do
  putStrLn "Hello"
  writeAt (10,10) "text"
  
