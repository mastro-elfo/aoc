type Codes = (Int, Int)

solution :: String -> Int
solution = maximum . map (codesToSeatId . parseLine) . lines

codesToSeatId :: Codes -> Int
codesToSeatId (a, b) = a * 8 + b

parseLine :: String -> Codes
parseLine line = (
    toInt . map (\x -> (if' (x == 'B') 1 0)) . take 7 $ line,
    toInt . map (\x -> (if' (x == 'R') 1 0)) . drop 7 $ line)

toInt :: [Int] -> Int
toInt [] = 0
toInt list = helper 1 (reverse list)
    where
        helper _ [] = 0
        helper n (1:xs) = n + helper (2*n) xs
        helper n (_:xs) = helper (2*n) xs

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
