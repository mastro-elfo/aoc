import System.IO
import Data.List

solution :: String -> Int
solution content = findTarget . sort . map read . lines $ content

findTarget :: [Int] -> Int
findTarget xs
    | result > 2020 = findTarget (init xs)
    | result < 2020 = findTarget (tail xs)
    | otherwise = fst * lst
    where
        fst = head xs
        lst = last xs
        result = fst + lst

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content