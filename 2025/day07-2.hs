import Data.List (elemIndex)
import Data.Map qualified
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)
import Debug.Trace (trace)

solution :: String -> Int
solution content = fst . propagate Data.Map.empty (findStart . head . lines $ content) $ zip [0 ..] (filter (any (/= '.')) . tail . lines $ content)

propagate :: Data.Map.Map (Int, Int) Int -> Int -> [(Int, String)] -> (Int, Data.Map.Map (Int, Int) Int)
propagate cache beam [] = (1, cache)
propagate cache beam ((row, line) : xs)
  | Data.Map.member key cache = (fromJust $ Data.Map.lookup key cache, cache)
  | (!! beam) line == '^' = (value, Data.Map.insert key value nextCache)
  | otherwise = propagate cache beam xs
  where
    key = (row, beam)
    (prev, prevCache) = propagate cache (beam - 1) xs
    (next, nextCache) = propagate prevCache (beam + 1) xs
    value = prev + next

add :: Int -> (Int, a) -> (Int, a)
add n (x, a) = (n + x, a)

findStart :: String -> Int
findStart = fromJust . elemIndex 'S'

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
