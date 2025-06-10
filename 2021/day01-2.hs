import System.IO

solution :: String -> Int
solution content = length . filter id . isIncreasing . windows . map read . lines $ content

windows :: [Int] -> [Int]
windows (x:y:z:[]) = [x+y+z]
windows (x:y:z:xs) = [x+y+z] ++ (windows ([y, z] ++ xs))

isIncreasing :: [Int] -> [Bool]
isIncreasing (x:y:[]) = [y > x]
isIncreasing (x:y:xs) = [y > x] ++ (isIncreasing ([y] ++ xs))

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content
