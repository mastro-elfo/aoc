import System.IO
import Data.Char

solution :: String -> Int
solution content = foldr (+) 0 . map parse . lines $  content

parse :: String -> Int
parse xs = 10 * (firstDigit xs) + (firstDigit (reverse xs))

firstDigit :: String -> Int
firstDigit (x:xs)
    | x `elem` "1234567890" = digitToInt x
    | otherwise = firstDigit xs

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content