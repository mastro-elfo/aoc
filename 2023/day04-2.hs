type Scratchcard = (Int, [Int], [Int], Int)

solution :: String -> Int
solution = sum . map fourth . play . map parse . lines

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

play :: [Scratchcard] -> [Scratchcard]
play [] = []
play ((a, b, c, d):xs) = (a, b, c, d) : play (map (\(e, f, g, h) -> (e, f, g, h+d)) (take wins xs) ++ drop wins xs)
    where
        wins = winning (a, b, c, d)

winning :: Scratchcard -> Int
winning (_, _, [], _) = 0
winning (i, xs, (y:ys), c)
    | y `elem` xs = 1 + winning (i, xs, ys, c)
    | otherwise = winning (i, xs, ys, c)

parse :: String -> Scratchcard
parse xs = (
    read .  takeWhile (/= ':') . tail . dropWhile (/= ' ') $ xs,
    map read . words . init . takeWhile (/= '|') . tail . dropWhile (/= ':') $ xs,
    map read . words . tail . dropWhile (/= '|') $ xs,
    1)

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
