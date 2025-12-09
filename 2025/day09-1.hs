type Coords = (Int, Int)

solution :: String -> Int
solution = maximum . areas . map parse . lines

areas :: [Coords] -> [Int]
areas [] = []
areas (x : xs) = map (area x) xs ++ areas xs

area :: Coords -> Coords -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

parse :: String -> Coords
parse line = (read . takeWhile (/= ',') $ line, read . tail . dropWhile (/= ',') $ line)

main :: IO ()
main = do readFile "day09.dat" >>= print . solution
