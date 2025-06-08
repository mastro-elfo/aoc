import System.IO

solution :: String -> Int
solution content = helper (map parse . lines $ content) 0 []

helper :: [Int] -> Int -> [Int] -> Int
helper freqs current found
    | elem newFreq found = newFreq
    | otherwise = helper (shift freqs) newFreq (found ++ [newFreq])
    where
        newFreq = current + (head freqs)

shift :: [Int] -> [Int]
shift xs = tail xs ++ [head xs]

parse :: String -> Int
parse (x:xs)
    | x == '+' = read xs :: Int
    | otherwise = - (read xs :: Int)

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content