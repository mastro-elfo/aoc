import System.IO
import Data.Char

solution :: String -> Int
solution content = foldr (+) 0 . map parse . lines $  content

parse :: String -> Int
parse xs = 10 * (firstDigit . replaceDigit False $ xs) + (firstDigit . reverse . replaceDigit True $ xs)

firstDigit :: String -> Int
firstDigit (x:xs)
    | x `elem` "1234567890" = digitToInt x
    | otherwise = firstDigit xs

replaceDigit :: Bool -> String -> String
replaceDigit False xs = 
    replace "one" "1"
    . replace "two" "2"
    . replace "three" "3"
    . replace "four" "4"
    . replace "five" "5"
    . replace "six" "6"
    . replace "seven" "7"
    . replace "eight" "8"
    . replace "nine" "9"
    . replace "zero" "0"
    . replace "twone" "2"
    . replace "zerone" "0"
    . replace "eightwo" "8"
    . replace "eighthree" "8"
    . replace "oneight" "1"
    . replace "nineight" "9"
    $ xs
replaceDigit True xs = 
    replaceDigit False
    . replace "twone" "1"
    . replace "zerone" "1"
    . replace "eightwo" "2"
    . replace "eighthree" "3"
    . replace "oneight" "8"
    . replace "nineight" "8"
    $ xs

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find repl s =
    if take (length find) s == find
        then repl ++ (replace find repl (drop (length find) s))
        else [head s] ++ (replace find repl (tail s))

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content