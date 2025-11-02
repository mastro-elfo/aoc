import Data.List (find, findIndex)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.Set (fromList, toList)

type Coord = (Int, Int)

type Guard = (Int, Int, Char)

solution :: String -> Int
solution content = (length . filter (\p -> play (p : obstacles) limits startPosition)) positions
  where
    rows = (length . lines) content
    startPosition = parseStartPosition rows content
    obstacles = parseObstacles rows content
    limits = parseLimits rows content
    positions = (toList . fromList . map (\(x, y, _) -> (x, y))) (track obstacles limits startPosition)

play :: [Coord] -> Coord -> Guard -> Bool
play obstacles limits current = helper [current] obstacles limits current
  where
    helper :: [Guard] -> [Coord] -> Coord -> Guard -> Bool
    helper positions obstacles limits current
      | isOutside limits newCurrent = False
      | newCurrent `elem` positions = True
      | isObstacle obstacles newCurrent = helper positions obstacles limits (rotate current)
      | otherwise = helper (newCurrent : positions) obstacles limits newCurrent
      where
        newCurrent = move current

track :: [Coord] -> Coord -> Guard -> [Guard]
track obstacles limits current = helper [current] obstacles limits current
  where
    helper :: [Guard] -> [Coord] -> Coord -> Guard -> [Guard]
    helper positions obstacles limits current
      | isOutside limits newCurrent = positions
      | isObstacle obstacles newCurrent = helper positions obstacles limits (rotate current)
      | otherwise = helper (newCurrent : positions) obstacles limits newCurrent
      where
        newCurrent = move current

rotate :: Guard -> Guard
rotate (x, y, '^') = (x, y, '>')
rotate (x, y, 'v') = (x, y, '<')
rotate (x, y, '<') = (x, y, '^')
rotate (x, y, '>') = (x, y, 'v')

move :: Guard -> Guard
move (x, y, '^') = (x, y - 1, '^')
move (x, y, 'v') = (x, y + 1, 'v')
move (x, y, '<') = (x - 1, y, '<')
move (x, y, '>') = (x + 1, y, '>')

isObstacle :: [Coord] -> Guard -> Bool
isObstacle obstacles (x, y, _) = isJust (find (\(ox, oy) -> x == ox && y == oy) obstacles)

isOutside :: Coord -> Guard -> Bool
isOutside (right, bottom) (x, y, _) = x < 0 || y < 0 || x > right || y > bottom

parseLimits :: Int -> String -> Coord
parseLimits rows content = indexToCoord rows (length (filter (/= '\n') content) - 1)

parseObstacles :: Int -> String -> [Coord]
parseObstacles rows content =
  catMaybes
    ( zipWith
        ( \index item ->
            if' (Just (indexToCoord rows index)) Nothing (item == '#')
        )
        [0 ..]
        (filter (/= '\n') content)
    )

parseStartPosition :: Int -> String -> Guard
parseStartPosition rows content = (x, y, d)
  where
    index = fromJust (findIndex (`elem` "^v<>") (filter (/= '\n') content))
    (x, y) = indexToCoord rows index
    d = fromJust (find (`elem` "^v<>") content)

indexToCoord :: Int -> Int -> Coord
indexToCoord rows index = (index `mod` rows, floor (fromIntegral index / fromIntegral rows))

if' :: a -> a -> Bool -> a
if' a _ True = a
if' _ a False = a

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
