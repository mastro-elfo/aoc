import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)

solution :: String -> Int
solution = helper [] . parse
  where
    helper :: [String] -> [Int] -> Int
    helper hs ms
      | hashed `elem` hs = fromMaybe 0 (elemIndex hashed hs) + 1
      | otherwise = helper (hashed : hs) reallocated
      where
        maxValue = maximum ms
        index = elemIndex (maximum ms) ms
        reallocated = reallocate (fromMaybe 0 index) ms
        hashed = show reallocated

reallocate :: Int -> [Int] -> [Int]
reallocate i xs = helper (i + 1) value emptied
  where
    value = (!! i) xs
    emptied = take i xs ++ [0] ++ drop (i + 1) xs
    helper :: Int -> Int -> [Int] -> [Int]
    helper i v ys
      | i == length ys = helper 0 v ys
      | v == 0 = ys
      | otherwise = helper (i + 1) (v - 1) (take i ys ++ [(!! i) ys + 1] ++ drop (i + 1) ys)

parse :: String -> [Int]
parse = map read . words

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
