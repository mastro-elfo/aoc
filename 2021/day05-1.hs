import Data.Maybe (isNothing, fromJust)
import Data.List (findIndex, find)

type Coords = (Int, Int)
type Vector = (Coords, Coords)

solution :: String -> Int
solution content = length . filter (\cc -> findTwo cc straights) $ [(x, y) | x<-[0..mx], y<-[0..my]]
    where
        straights = filter isStraight . map parse . lines $ content
        (mx, my) = getLimit straights


findOne :: Coords -> [Vector] -> Bool
findOne coords vectors
    | isNothing element = False
    | otherwise = True
    where
        element = find (\v -> isInLine coords v) vectors

findTwo :: Coords -> [Vector] -> Bool
findTwo coords vectors
    | isNothing index = False
    | otherwise = findOne coords (drop (fromJust index +1) vectors)
    where
        index = findIndex (\v -> isInLine coords v) vectors

isInLine :: Coords -> Vector -> Bool
isInLine (x, y) ((sx, sy), (ex, ey))
    | allEqual x sx ex = isBetween y sy ey
    | allEqual y sy ey = isBetween x sx ex
    | otherwise = False

isBetween :: (Ord a) => a -> a -> a -> Bool
isBetween x left right = left <= x && x <= right || right <= x && x <= left

allEqual :: (Eq a) => a -> a -> a -> Bool
allEqual x y z = x == y && y == z

getLimit :: [Vector] -> Coords
getLimit = foldl (\(x, y) ((sx, sy), (ex, ey)) -> (maximum [x, sx, ex], maximum [y, sy, ey])) (0, 0)

isStraight :: Vector -> Bool
isStraight ((sx, sy), (ex, ey)) = sx == ex || sy == ey

parse :: String -> Vector
parse line = ((sx, sy), (ex, ey))
    where
        sx = read . takeWhile (/= ',') $ line
        sy = read . takeWhile (/= ' ') . tail . dropWhile (/= ',') $ line
        ex = read . takeWhile (/= ',') . tail . dropWhile (/= '>') $ line
        ey = read . tail . dropWhile (/= ',') . tail . dropWhile (/= ',') $ line

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
