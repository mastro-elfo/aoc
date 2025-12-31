import Data.List (isInfixOf, maximumBy, minimumBy, sort)
import Data.Maybe (isJust)
import Data.Set (fromList, toList)

type DateTime = (Int, Int, Int, Int, Int)

type Guard = (Int, Maybe DateTime, Maybe DateTime)

solution :: String -> Int
solution content = worstId * worstMinute
  where
    guards = filter (isJust . guardAsleep) . parse . lines $ content
    guardIds = unique . map guardId $ guards
    worstId =
      fst
        . maximumBy second
        . map (\gid -> (gid, totAsleep guards gid))
        $ guardIds
    worstGuardShifts = filter ((== worstId) . guardId) guards
    worstMinute =
      fst
        . maximumBy second
        . map (\m -> (m, countBetween m worstGuardShifts))
        $ [0 .. 59]

countBetween :: Int -> [Guard] -> Int
countBetween m = length . filter (\(_, start, end) -> isBetween m start end)

second :: (Ord b) => (a, b) -> (a, b) -> Ordering
second (_, b1) (_, b2) = compare b1 b2

totAsleep :: [Guard] -> Int -> Int
totAsleep gs gid = sum . map (\(_, start, end) -> difference start end) . filter ((== gid) . guardId) $ gs

unique :: (Ord a) => [a] -> [a]
unique = toList . fromList

isBetween :: Int -> Maybe DateTime -> Maybe DateTime -> Bool
isBetween _ Nothing _ = False
isBetween _ _ Nothing = False
isBetween m (Just (y1, m1, d1, h1, p1)) (Just (y2, m2, d2, h2, p2))
  | h1 == h2 = p1 <= m && m < p2
  | otherwise = p1 <= m || m < p2

difference :: Maybe DateTime -> Maybe DateTime -> Int
difference (Just dt1) (Just dt2)
  | dt1 == dt2 = 0
  | year == year2 && month == month2 && day == day2 && hour == hour2 = minute2 - minute
  | year == year2 && month == month2 && day == day2 = (hour2 - hour) * 60 + minute2 - minute
  | year == year2 && month == month2 = (day2 - day) * 1440 + (hour2 - hour) * 60 + minute2 - minute
  | otherwise = 1440 + difference (dtnorm (year, month, day + 1, hour, minute)) (Just dt2)
  where
    (year, month, day, hour, minute) = dt1
    (year2, month2, day2, hour2, minute2) = dt2

dtnorm :: DateTime -> Maybe DateTime
dtnorm (year, month, day, hour, 60) = dtnorm (year, month, day, hour + 1, 0)
dtnorm (year, month, day, 24, minute) = dtnorm (year, month, day + 1, 0, minute)
dtnorm (year, 1, 32, hour, minute) = dtnorm (year, 2, 1, hour, minute)
dtnorm (year, 2, 29, hour, minute) = dtnorm (year, 3, 1, hour, minute)
dtnorm (year, 3, 32, hour, minute) = dtnorm (year, 4, 1, hour, minute)
dtnorm (year, 4, 31, hour, minute) = dtnorm (year, 5, 1, hour, minute)
dtnorm (year, 5, 32, hour, minute) = dtnorm (year, 6, 1, hour, minute)
dtnorm (year, 6, 31, hour, minute) = dtnorm (year, 7, 1, hour, minute)
dtnorm (year, 7, 32, hour, minute) = dtnorm (year, 8, 1, hour, minute)
dtnorm (year, 8, 32, hour, minute) = dtnorm (year, 9, 1, hour, minute)
dtnorm (year, 9, 31, hour, minute) = dtnorm (year, 10, 1, hour, minute)
dtnorm (year, 10, 32, hour, minute) = dtnorm (year, 11, 1, hour, minute)
dtnorm (year, 11, 31, hour, minute) = dtnorm (year, 12, 1, hour, minute)
dtnorm (year, 12, 32, hour, minute) = dtnorm (year + 1, 1, 1, hour, minute)
dtnorm dt = Just dt

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

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
