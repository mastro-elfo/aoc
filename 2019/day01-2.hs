import System.IO

solution :: String -> Int
solution content = foldr (+) 0 . map getFuel . map read . lines $ content

getFuel :: Int -> Int
getFuel x
    | x <= 0 = 0
    | otherwise = fuel + getFuel fuel
        where
            fuel = max 0 ((div x 3) - 2)

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content