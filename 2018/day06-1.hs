import Data.List (elemIndex)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Set (fromList, toList)

type Coords = (Int, Int)

solution :: String -> Int
solution content = maximum . map (\f -> length . filter (== f) $ noBord) $ dedup noBord
  where
    pivots = map parse . lines $ content
    hlimit = maximum . map fst $ pivots
    vlimit = maximum . map snd $ pivots
    theMap = [nearest (x, y) pivots | x <- [0 .. hlimit], y <- [0 .. vlimit]]
    borders = dedup . borderlines vlimit $ theMap
    noBord = filter (`notElem` borders) $ catMaybes theMap

dedup :: (Ord a) => [a] -> [a]
dedup = toList . fromList

borderlines :: Int -> [Maybe Coords] -> [Coords]
borderlines vlimit xs = catMaybes (one ++ two ++ thr ++ fou)
  where
    one = take (vlimit + 1) xs
    two = take (vlimit + 1) (reverse xs)
    thr = extract (vlimit + 1) xs
    fou = extract (vlimit + 1) (drop 1 xs)

extract :: Int -> [a] -> [a]
extract n = helper (n - 1)
  where
    helper _ [] = []
    helper 0 (x : xs) = x : helper (n - 1) xs
    helper m (x : xs) = helper (m - 1) xs

nearest :: Coords -> [Coords] -> Maybe Coords
nearest c ps
  | (length . filter (== minDist) $ distances) > 1 = Nothing
  | otherwise = Just ((!! (fromMaybe 0 . elemIndex minDist $ distances)) ps)
  where
    distances = map (manhattan c) ps
    minDist = minimum distances

manhattan :: Coords -> Coords -> Int
manhattan (c1x, c1y) (c2x, c2y) = abs (c1x - c2x) + abs (c1y - c2y)

parse :: String -> Coords
parse xs =
  ( read . init . head . words $ xs,
    read . last . words $ xs
  )

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
