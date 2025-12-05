import Data.List (find)
import Data.Maybe (fromJust, isJust)

type Range = (Int, Int)

solution :: String -> Int
solution = sum . map size . reduce . map parseRange . takeWhile (/= "") . lines

size :: Range -> Int
size (start, end) = end - start + 1

reduce :: [Range] -> [Range]
reduce [] = []
reduce (r : rs)
  | isJust other = reduce (merge r justOther : (filter (/= justOther) . filter (/= r) $ rs))
  | otherwise = r : reduce rs
  where
    other = find (isMergable r) rs
    justOther = fromJust other

isMergable :: Range -> Range -> Bool
isMergable a b = helper a b || helper b a
  where
    helper (start1, end1) (start2, end2) = start2 <= start1 && start1 <= end2 || start1 <= start2 && end2 <= end1

merge :: Range -> Range -> Range
merge (start1, end1) (start2, end2) = (min start1 start2, max end1 end2)

parseRange :: String -> Range
parseRange line = (read . takeWhile (/= '-') $ line, read . tail . dropWhile (/= '-') $ line)

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
