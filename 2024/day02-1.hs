import System.IO

solution :: String -> Int
solution = length . filter id . map isSafe . map (map read) . map words . lines

isSafe :: [Int] -> Bool
isSafe xs = (isWithinRange xs) && ((isIncreasing xs) || (isDecreasing xs))

isIncreasing :: [Int] -> Bool
isIncreasing xs = all (\(a, b) -> b > a) (zip xs (tail xs))

isDecreasing :: [Int] -> Bool
isDecreasing xs = all (\(a, b) -> b < a) (zip xs (tail xs))

isWithinRange :: [Int] -> Bool
isWithinRange xs = all (\(a, b) -> 1<=abs(a-b) && abs(a-b)<=3) (zip xs (tail xs))

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content