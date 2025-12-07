import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)

solution :: String -> Int
solution content = snd . foldl propagate ([findStart . head . lines $ content], 0) $ (filter (any (/= '.')) . tail . lines $ content)

propagate :: ([Int], Int) -> String -> ([Int], Int)
propagate (beams, count) line =
  ( split beams splitters,
    count + (length . filter (`elem` splitters) $ beams)
  )
  where
    splitters = findSplitters line

split :: [Int] -> [Int] -> [Int]
split beams splitters = unique (filter (`notElem` splitters) beams ++ concatMap (\s -> [s - 1, s + 1]) splitters)

findSplitters :: String -> [Int]
findSplitters = map fst . filter ((== '^') . snd) . zip [0 ..]

findStart :: String -> Int
findStart = fromJust . elemIndex 'S'

unique :: [Int] -> [Int]
unique = toList . fromList

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
