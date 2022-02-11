module Main where
import Text.Read (readEither)

getDigit :: String -> IO Int
getDigit prompt = do putStrLn prompt
                     x <- getLine
                     case readEither x of
                       Right s -> return s
                       Left s -> do putStrLn "ERROR: Invalid digit"
                                    getDigit prompt

adder :: IO ()
adder = do
  amount <- getDigit "How many numbers?"
  digits <- sequence [getDigit "" | _ <- [1..amount]]
  putStrLn $ "The total sum is: " ++ show (sum digits)

readLine :: IO String
readLine = do c <- getChar
              case c of
                '\DEL' -> do putChar 'b'
                             readLine
                '\n' -> return ""
                other -> do next <- readLine
                            return (c : next)

main :: IO ()
main = do line <- readLine
          putStrLn line

