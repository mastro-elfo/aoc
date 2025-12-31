import Data.List (isInfixOf, maximumBy, sort)
import Data.Maybe (isJust)
import Data.Set (fromList, toList)

type DateTime = (Int, Int, Int, Int, Int)

type Guard = (Int, Maybe DateTime, Maybe DateTime)

solution :: String -> Int
solution content = gid * minute
  where
    guards = filter (isJust . guardAsleep) . parse . lines $ content
    guardIds = unique . map guardId $ guards
    (gid, minute, _) = maximumBy third . map (\(gid, minute) -> (gid, minute, countBetween minute gid guards)) $ [(gid, minute) | gid <- guardIds, minute <- [0 .. 59]]

third :: (Ord c) => (a, b, c) -> (a, b, c) -> Ordering
third (_, _, c1) (_, _, c2) = compare c1 c2

countBetween :: Int -> Int -> [Guard] -> Int
countBetween m g = length . filter (\(gid, start, end) -> gid == g && isBetween m start end)

isBetween :: Int -> Maybe DateTime -> Maybe DateTime -> Bool
isBetween _ Nothing _ = False
isBetween _ _ Nothing = False
isBetween m (Just (y1, m1, d1, h1, p1)) (Just (y2, m2, d2, h2, p2))
  | h1 == h2 = p1 <= m && m < p2
  | otherwise = p1 <= m || m < p2

toDateTime :: String -> DateTime
toDateTime (y1 : y2 : y3 : y4 : '-' : m1 : m2 : '-' : d1 : d2 : ' ' : h1 : h2 : ':' : p1 : p2 : "") = (read (y1 : y2 : y3 : y4 : ""), read (m1 : m2 : ""), read (d1 : d2 : ""), read (h1 : h2 : ""), read (p1 : p2 : ""))

parse :: [String] -> [Guard]
parse = foldl helper [] . sort
  where
    helper gs line
      | "wakes up" `isInfixOf` line = (guardId . head $ gs, Nothing, Nothing) : (guardId . head $ gs, guardAsleep . head $ gs, parseDateTime line) : tail gs
      | "falls asleep" `isInfixOf` line = (guardId . head $ gs, parseDateTime line, Nothing) : tail gs
      | otherwise = (parseGuardId line, Nothing, Nothing) : gs

parseGuardId :: String -> Int
parseGuardId = read . tail . (!! 3) . words

parseDateTime :: String -> Maybe DateTime
parseDateTime = Just . toDateTime . takeWhile (/= ']') . tail

guardId :: Guard -> Int
guardId (id, _, _) = id

guardAsleep :: Guard -> Maybe DateTime
guardAsleep (_, asleep, _) = asleep

guardAwake :: Guard -> Maybe DateTime
guardAwake (_, _, awake) = awake

unique :: (Ord a) => [a] -> [a]
unique = toList . fromList

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
