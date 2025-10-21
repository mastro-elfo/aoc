import Data.Set (fromList)

solution :: String -> Int
solution = sum . map (length . fromList) . group . lines

group :: [String] -> [String]
group = helper []
  where
    helper o [] = o
    helper o (x : xs)
      | x == "" = helper (o ++ [[]]) xs
      | null o = helper [x] xs
      | otherwise = helper (init o ++ [last o ++ x]) xs

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
