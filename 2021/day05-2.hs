import Data.Maybe (isNothing, fromJust)
import Data.List (findIndex, find)

type Coords = (Int, Int)
type Vector = (Coords, Coords)

solution :: String -> Int
solution content = length . filter (\cc -> findTwo cc vectors) $ [(x, y) | x<-[0..mx], y<-[0..my]]
    where
        vectors = map parse . lines $ content
        (mx, my) = getLimit vectors


findOne :: Coords -> [Vector] -> Bool
findOne _ [] = False
findOne c (x:xs)
    | isInLine c x = True
    | otherwise = findOne c xs

findTwo :: Coords -> [Vector] -> Bool
findTwo _ [] = False
findTwo c (x:xs)
    | isInLine c x = findOne c xs
    | otherwise = findTwo c xs

isInLine :: Coords -> Vector -> Bool
isInLine coords vector
    | allEqual x sx ex = isBetween y sy ey
    | allEqual y sy ey = isBetween x sx ex
    | isDiagonal coords vector = isBetween x sx ex
    | otherwise = False
    where
        (x, y) = coords
        ((sx, sy), (ex, ey)) = vector

isDiagonal :: Coords -> Vector -> Bool
isDiagonal (x, y) ((sx, sy), (ex, ey)) = (sy - sx == ey - ex && ey - ex == y - x) || (sx + sy == ex + ey && ex + ey == x + y)

isBetween :: (Ord a) => a -> a -> a -> Bool
isBetween x left right = left <= x && x <= right || right <= x && x <= left

allEqual :: (Eq a) => a -> a -> a -> Bool
allEqual x y z = x == y && y == z

getLimit :: [Vector] -> Coords
getLimit = foldl (\(x, y) ((sx, sy), (ex, ey)) -> (maximum [x, sx, ex], maximum [y, sy, ey])) (0, 0)

parse :: String -> Vector
parse line = ((sx, sy), (ex, ey))
    where
        sx = read . takeWhile (/= ',') $ line
        sy = read . takeWhile (/= ' ') . tail . dropWhile (/= ',') $ line
        ex = read . takeWhile (/= ',') . tail . dropWhile (/= '>') $ line
        ey = read . tail . dropWhile (/= ',') . tail . dropWhile (/= ',') $ line

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
