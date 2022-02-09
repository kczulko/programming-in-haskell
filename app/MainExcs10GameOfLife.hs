module Main where
import GHC.IO.Handle.Internals (wantWritableHandle)

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)
type Board = [Pos]

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

showcells :: Board -> IO ()
showcells board = sequence_ [writeAt pos "o" | pos <- board]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not $ isAlive b p

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1),
                          (x,y+1), (x+1,y+1)
                         ]
                where
                  wrap :: Pos -> Pos
                  wrap (x,y) = (((x-1) `mod` width) + 1,
                                ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2,3]]

births :: Board -> [Pos]
births b = [(x,y) | x <- [1..width],
                    y <- [1..height],
                    pos <- [(x,y)],
                    isEmpty b pos,
                    liveneighbs b pos == 3]

rmdups :: Board -> Board
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)
          where
            wait n = sequence_ [return () | _ <- [1..n]]

putStr2 :: String -> IO ()
putStr2 s = sequence_ [putChar c | c <- s]

main :: IO ()
main = life glider
       where
         glider = [(4,2), (2,3), (4,3), (3,4), (4,4) ]

  -- do
  -- putStrLn "Hello"
  -- writeAt (10,10) "text"

