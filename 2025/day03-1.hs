import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust)

solution :: String -> Int
solution = sum . (map ((toInt . extract 2) . parse) . lines)

toInt :: [Int] -> Int
toInt = foldl (\x -> (+) (10 * x)) 0

extract :: Int -> [Int] -> [Int]
extract 0 _ = []
extract n xs = first : extract (n - 1) (drop (firstIndex + 1) xs)
  where
    first = maximum . take (length xs - n + 1) $ xs
    firstIndex = fromJust . elemIndex first $ xs

parse :: String -> [Int]
parse "" = []
parse (x : xs) = read (x : "") : parse xs

main :: IO ()
main = do readFile "day03.dat" >>= print . solution
