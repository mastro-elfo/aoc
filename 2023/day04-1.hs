-- solution :: String -> Int
solution =  sum . map ((2^) . subtract 1) . filter (/= 0) . map (winning . parse) . lines

winning :: ([Int], [Int]) -> Int
winning (_, []) = 0
winning (xs, (y:ys))
    | y `elem` xs = 1 + winning (xs, ys)
    | otherwise = winning (xs, ys)

parse :: String -> ([Int], [Int])
parse xs = (
    map read . words . init . takeWhile (/= '|') . tail . dropWhile (/= ':') $ xs,
    map read . words . tail . dropWhile (/= '|') $ xs)

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
