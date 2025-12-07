import Data.List (elemIndex)
import Data.Map (Map, empty, insert, lookup, member)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

type Cache = Map (Int, Int) Int

solution :: String -> Int
solution content = fst . propagate empty (findStart . head . lines $ content) $ zip [0 ..] (filter (any (/= '.')) . tail . lines $ content)

propagate :: Cache -> Int -> [(Int, String)] -> (Int, Cache)
propagate cache beam [] = (1, cache)
propagate cache beam ((row, line) : xs)
  | member key cache = (fromJust $ lookup key cache, cache)
  | (!! beam) line == '^' = (value, insert key value nextCache)
  | otherwise = propagate cache beam xs
  where
    key = (row, beam)
    (prev, prevCache) = propagate cache (beam - 1) xs
    (next, nextCache) = propagate prevCache (beam + 1) xs
    value = prev + next

findStart :: String -> Int
findStart = fromJust . elemIndex 'S'

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
