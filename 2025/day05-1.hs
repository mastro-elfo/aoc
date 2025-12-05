import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (traceShow)

type Range = (Int, Int)

type Id = Int

solution :: String -> Int
solution content = length . filter (isFresh ranges) $ ids
  where
    ranges = reduce . map parseRange . takeWhile (/= "") . lines $ content
    ids = map read . tail . dropWhile (/= "") . lines $ content

isFresh :: [Range] -> Id -> Bool
isFresh rs id = any (isWithin id) rs

isWithin :: Id -> Range -> Bool
isWithin id (start, end) = start <= id && id <= end

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
