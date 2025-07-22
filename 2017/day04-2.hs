import Data.List (sort)
import Data.Set (fromList, size)

solution :: String -> Int
solution = length . filter isValid . lines

isValid :: String -> Bool
isValid xs =
  (length . words $ xs) == (size . fromList . words $ xs) && (not . any isAnagram) [(word, [w | w <- words xs, w /= word]) | word <- words xs]

isAnagram :: (String, [String]) -> Bool
isAnagram (word, xs) = helper (sort word) xs
  where
    helper _ [] = False
    helper srtd (x : xs)
      | srtd == sort x = True
      | otherwise = helper srtd xs

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
