import System.IO

solution :: String -> Int
solution content = helper 0 1 content

helper :: Int -> Int -> String -> Int
helper current index (x:xs)
    | current == 0 && x == ')' = index
    | x == '(' = helper (current + 1) (index + 1) xs
    | otherwise = helper (current - 1) (index + 1) xs

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . show . solution $ content