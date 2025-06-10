import System.IO

solution :: String -> Int
solution content = length . filter id . isIncreasing . map read . lines $ content

isIncreasing :: [Int] -> [Bool]
isIncreasing (x:y:[]) = [y > x]
isIncreasing (x:y:xs) = [y > x] ++ (isIncreasing ([y] ++ xs))

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content
