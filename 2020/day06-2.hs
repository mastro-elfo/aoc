import Data.Set (fromList, toList)

solution :: String -> Int
solution = sum . map (length . everyYes) . group . lines

everyYes :: [String] -> String
everyYes group = filter (\x -> all (\p -> x `elem` p) group) (toList . fromList . unwords $ group)

group :: [String] -> [[String]]
group = helper []
  where
    helper o [] = o
    helper o (x : xs)
      | x == "" = helper (o ++ [[]]) xs
      | null o = helper [[x]] xs
      | otherwise = helper (init o ++ [last o ++ [x]]) xs

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
