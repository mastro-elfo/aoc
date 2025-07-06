import Data.Char (ord)

solution :: String -> Int
solution = sum . map (priority . badge) . groupBy . lines

badge :: (String, String, String) -> Char
badge (x : xs, b, c)
  | x `elem` b && x `elem` c = x
  | otherwise = badge (xs, b, c)

groupBy :: [String] -> [(String, String, String)]
groupBy (a : b : c : xs) = (a, b, c) : groupBy xs
groupBy [] = []

priority :: Char -> Int
priority c
  | o >= ord 'a' && o <= ord 'z' = o - ord 'a' + 1
  | o >= ord 'A' && o <= ord 'Z' = o - ord 'A' + 27
  | otherwise = 0
  where
    o = ord c

main :: IO ()
main = do readFile "day03.dat" >>= print . solution
