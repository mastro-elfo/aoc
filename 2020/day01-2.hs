import System.IO
import Data.List
import Data.Maybe

solution :: String -> Int
solution content = helper expenses expenses
    where
        expenses = sort . map read . lines $ content

helper xs (y:ys)
    | isNothing r = helper xs ys
    | otherwise = (fromJust r) * y
    where
        r = findTarget (2020 - y) xs

findTarget :: Int -> [Int] -> Maybe Int
findTarget t xs
    | length xs < 2 = Nothing
    | result > t = findTarget t (init xs)
    | result < t = findTarget t (tail xs)
    | otherwise = Just (fst * lst)
    where
        fst = head xs
        lst = last xs
        result = fst + lst

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content