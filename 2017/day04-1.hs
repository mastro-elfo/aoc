import Data.Set (fromList, size)

solution :: String -> Int
solution = length . filter isValid . lines

isValid :: String -> Bool
isValid xs = (length . words $ xs) == (size . fromList . words $ xs)

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
