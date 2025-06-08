import System.IO

solution :: String -> Int
solution content = foldr (+) 0 . map parse . lines $ content

parse :: String -> Int
parse (x:xs)
    | x == '+' = read xs :: Int
    | otherwise = - (read xs :: Int)

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content