{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

type Table = (Int, Int, Int)

solution :: String -> Int
solution content = minimum . map (flip (foldl convert) $ maps) $ seeds
  where
    seeds :: [Int] = map read . words . dropWhile (/= ' ') . head . lines $ content
    maps :: [[Table]] = parseMaps . drop 2 . lines $ content

parseMaps :: [String] -> [[Table]]
parseMaps = helper []
  where
    helper :: [[Table]] -> [String] -> [[Table]]
    helper ts [] = ts
    helper ts ("" : xs) = helper ts xs
    helper ts (x : xs)
      | '-' `elem` x = helper (ts ++ [[]]) xs
      | otherwise = helper (init ts ++ [last ts ++ [parseTable x]]) xs

parseTable :: String -> Table
parseTable line = (head values, values !! 1, values !! 2)
  where
    values :: [Int] = map read . words $ line

convert :: Int -> [Table] -> Int
convert v [] = v
convert v (t : ts)
  | isIn t v = conversion t v
  | otherwise = convert v ts

conversion :: Table -> Int -> Int
conversion (dst, src, _) v = dst + v - src

isIn :: Table -> Int -> Bool
isIn (_, src, size) value = src <= value && value < src + size

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
