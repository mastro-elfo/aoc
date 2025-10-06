import Data.List (sortOn)
import Data.Ord qualified

solution :: String -> String
solution content = freq
  where
    columns = transpose . lines $ content
    columnsChars = map dedup columns
    freq = zipWith (curry (fst . (!! 0) . sortOn (Data.Ord.Down . snd) . frequency)) columns columnsChars

frequency :: (Ord a) => ([a], [a]) -> [(a, Int)]
frequency = helper []
  where
    helper xs (_, []) = xs
    helper xs (columns, y : ys) = helper ((y, count y columns) : xs) (columns, ys)

count :: (Eq a) => a -> [a] -> Int
count a [] = 0
count a (x : xs)
  | a == x = 1 + count a xs
  | otherwise = count a xs

transpose :: [[a]] -> [[a]]
transpose zs = helper (replicate (length . (!! 0) $ zs) []) zs
  where
    helper xs [] = xs
    helper xs (y : ys) = helper (zipWith (:) y xs) ys

dedup :: (Eq a) => [a] -> [a]
dedup [] = []
dedup (x : xs)
  | x `elem` xs = dedup xs
  | otherwise = x : dedup xs

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
