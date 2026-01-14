import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set (fromList, toList)
import Distribution.Utils.Generic (trdOf3)

type Coords = (Int, Int)

type Element = (Coords, Coords, Int)

solution :: String -> Int
solution content =
  trdOf3
    . fromJust
    . find
      (isValid reds)
    -- . filter ((== 1560475800) . trdOf3)
    . sortBy (comparing Down `on` trdOf3)
    $ areas
  where
    reds = map parse . lines $ content
    areas = allAreas reds

isValid :: [Coords] -> Element -> Bool
isValid reds ((x1, y1), (x2, y2), a) =
  all
    (\item -> isRed reds item || isSideGreen reds item || isInsideGreen reds item)
    . filter (\(x, y) -> min x1 x2 <= x && x <= max x1 x2 && min y1 y2 <= y && y <= max y1 y2)
    $ ( (unique . neighbors $ reds)
          ++ ( neighbors
                 . catMaybes
                 $ [ s1 `intersect` s2
                   | s1 <- zip reds (last reds : init reds),
                     s2 <- [((x1, y1), (x1, y2)), ((x1, y2), (x2, y2)), ((x2, y2), (x2, y1)), ((x2, y1), (x1, y1))]
                   ]
             )
      )

intersect :: (Coords, Coords) -> (Coords, Coords) -> Maybe Coords
intersect ((a1x, a1y), (a2x, a2y)) ((b1x, b1y), (b2x, b2y))
  | a1x == a2x && b1y == b2y && isBetween (a1x, b1y) ((a1x, a1y), (a2x, a2y)) && isBetween (a1x, b1y) ((b1x, b1y), (b2x, b2y)) = Just (a1x, b1y)
  | a1y == a2y && b1x == b2x && isBetween (b1x, a1y) ((a1x, a1y), (a2x, a2y)) && isBetween (b1x, a1y) ((b1x, b1y), (b2x, b2y)) = Just (b1x, a1y)
  | otherwise = Nothing

isBetween :: Coords -> (Coords, Coords) -> Bool
isBetween (x, y) ((sx, sy), (ex, ey))
  | sx == ex && sx == x = min sy ey < y && y < max sy ey
  | sy == ey && sy == y = min sx ex < x && x < max sx ex
  | otherwise = False

neighbors :: [Coords] -> [Coords]
neighbors [] = []
neighbors ((x, y) : rest) =
  [ (x - 1, y - 1),
    (x - 1, y),
    (x - 1, y + 1),
    (x, y - 1),
    (x, y),
    (x, y + 1),
    (x + 1, y - 1),
    (x + 1, y),
    (x + 1, y + 1)
  ]
    ++ neighbors rest

isRed :: [Coords] -> Coords -> Bool
isRed reds x = x `elem` reds

isSideGreen :: [Coords] -> Coords -> Bool
isSideGreen reds (x, y) =
  any
    ( \((sx, sy), (ex, ey)) ->
        (min sx ex <= x && x <= max sx ex && sy == y && y == ey)
          || (sx == x && x == ex && min sy ey <= y && y <= max sy ey)
    )
    $ zip reds (last reds : init reds)

isInsideGreen :: [Coords] -> Coords -> Bool
isInsideGreen reds (x, y) =
  (== 1)
    . (`mod` 2)
    . length
    . filter (\((sx, sy), (ex, ey)) -> sy == ey && ey > y && min sx ex <= x && x < max sx ex)
    $ zip reds (last reds : init reds)

allAreas :: [Coords] -> [Element]
allAreas [] = []
allAreas (x : xs) = map (\y -> (x, y, area x y)) xs ++ allAreas xs

area :: Coords -> Coords -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

parse :: String -> Coords
parse line = (read . takeWhile (/= ',') $ line, read . drop 1 . dropWhile (/= ',') $ line)

unique :: (Ord a) => [a] -> [a]
unique = toList . fromList

main :: IO ()
main = do readFile "day09.dat" >>= print . solution
