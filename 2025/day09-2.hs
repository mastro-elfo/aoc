import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set (fromList, toList)

type Coords = (Int, Int)

type Segment = (Coords, Coords)

type Rectangle = (Coords, Coords)

solution :: String -> Int
solution content =
  snd
    . fromJust
    . find (isValidRectangle grid . fst)
    . sortBy (comparing Down `on` snd)
    . generate
    $ vertices
  where
    vertices = map parse . lines $ content
    sides = zip vertices (drop 1 vertices ++ take 1 vertices)
    horizontals = filter isHorizontal sides
    grid =
      unique
        . map (\((x1, y1), (x2, y2)) -> ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2)))
        . filter (not . isInternalRectangle (vertices, sides, horizontals))
        $ zip vertices (drop 2 vertices ++ take 2 vertices)

isValidRectangle :: [Rectangle] -> Rectangle -> Bool
isValidRectangle rs r = not (any (overlap r) rs)

overlap :: Rectangle -> Rectangle -> Bool
overlap rect1 rect2 = helper rect1 rect2 || helper rect2 rect1
  where
    helper rect ((x1, y1), (x2, y2)) =
      helper2 rect ((x1, y1), (x2, y2))
        || any
          (helper1 rect)
          [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]
    helper1 ((x1, y1), (x2, y2)) (x, y) = min x1 x2 < x && x < max x1 x2 && min y1 y2 < y && y < max y1 y2
    helper2 ((a1, b1), (a2, b2)) ((c1, d1), (c2, d2)) = min a1 a2 <= min c1 c2 && max c1 c2 <= max a1 a2 && min d1 d2 <= min b1 b2 && max b1 b2 <= max d1 d2

generate :: [Coords] -> [(Rectangle, Int)]
generate [] = []
generate (x : xs) = map (\y -> ((x, y), area (x, y))) xs ++ generate xs

area :: Rectangle -> Int
area ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

isInternalRectangle :: ([Coords], [Segment], [Segment]) -> Rectangle -> Bool
isInternalRectangle dt ((x1, y1), (x2, y2)) =
  all
    (isInternal dt)
    [(xmin + 1, ymin + 1), (xmax - 1, ymin + 1), (xmax - 1, ymax - 1), (xmin + 1, ymax - 1)]
  where
    xmin = min x1 x2
    xmax = max x1 x2
    ymin = min y1 y2
    ymax = max y1 y2

isInternal :: ([Coords], [Segment], [Segment]) -> Coords -> Bool
isInternal (vertices, segments, horizontals) coords =
  isRed vertices coords
    || isSideGreen segments coords
    || isInsideGreen horizontals coords

isRed :: [Coords] -> Coords -> Bool
isRed vs x = x `elem` vs

isSideGreen :: [Segment] -> Coords -> Bool
isSideGreen [] _ = False
isSideGreen (segment : xs) (x, y)
  | isHorizontal segment = y == y1 && isBetween x x1 x2 || isSideGreen xs (x, y)
  | isVertical segment = x == x1 && isBetween y y1 y2 || isSideGreen xs (x, y)
  where
    ((x1, y1), (x2, y2)) = segment

isInsideGreen :: [Segment] -> Coords -> Bool
isInsideGreen [] _ = False
isInsideGreen xs (x, y) = odd . length . filter (\((x1, y1), (x2, _)) -> y1 < y && isBetween x x1 x2) $ xs

isBetween :: Int -> Int -> Int -> Bool
isBetween x s e = min s e < x && x < max s e

isHorizontal :: Segment -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

isVertical :: Segment -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

unique :: (Ord a) => [a] -> [a]
unique = toList . fromList

parse :: String -> Coords
parse xs = (read . takeWhile (/= ',') $ xs, read . drop 1 . dropWhile (/= ',') $ xs)

main :: IO ()
main = do readFile "day09.dat" >>= print . solution
