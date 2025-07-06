import Data.Char (ord)

solution :: String -> Int
solution = sum . map (priority . repeated) . lines

repeated :: String -> Char
repeated xs = helper (take len xs) (drop len xs)
  where
    len = length xs `div` 2
    helper (x : xs) other
      | x `elem` other = x
      | otherwise = helper xs other

priority :: Char -> Int
priority c
  | o >= ord 'a' && o <= ord 'z' = o - ord 'a' + 1
  | o >= ord 'A' && o <= ord 'Z' = o - ord 'A' + 27
  | otherwise = 0
  where
    o = ord c

main :: IO ()
main = do readFile "day03.dat" >>= print . solution
