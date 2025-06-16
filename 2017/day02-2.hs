import System.IO

solution :: String -> Int
solution content = foldr (+) 0 . map rowExactDivision . map (map read) . map words . lines $ content

rowExactDivision :: [Int] -> Int
rowExactDivision xs = helper (generator xs)
    where
        helper :: [(Int, Int)] -> Int
        helper ((x,y):xs)
            | mod x y == 0 = div x y
            | otherwise = helper xs

generator :: [Int] -> [(Int, Int)]
generator xs = [(xs!!i, xs!!j) | i <- take (length xs) [0..], j <- take (length xs) [0..], i /= j]

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content