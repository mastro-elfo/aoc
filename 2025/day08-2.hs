import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Set (fromList, toList)
import Data.Text (pack, splitOn, unpack)
import Distribution.Utils.Generic (trdOf3)

type Coords = (Int, Int, Int)

type Distances = (Coords, Coords, Float)

type Circuit = [Coords]

solution :: String -> Int
solution content = multiplyX . connect (length boxes) . map (\(a, b, _) -> [a, b]) . sortBy (compare `on` trdOf3) . toDistances $ boxes
  where
    boxes = map parse . lines $ content

connect :: Int -> [Circuit] -> (Coords, Coords)
connect n (cs : css)
  | isComplete = (head jOther, last jOther)
  | isJust other = connect n (unique (cs ++ jOther) : filter (/= jOther) css)
  | otherwise = connect n (css ++ [cs])
  where
    other = find (\os -> any (`elem` os) cs) css
    jOther = fromJust other
    newConfig = unique (cs ++ jOther) : filter (/= jOther) css
    isComplete = isJust other && ((length . head $ newConfig) == n)

multiplyX :: (Coords, Coords) -> Int
multiplyX ((x1, _, _), (x2, _, _)) = x1 * x2

unique :: (Ord a) => [a] -> [a]
unique = toList . fromList

toDistances :: [Coords] -> [Distances]
toDistances [] = []
toDistances (x : xs) = map (\y -> (x, y, euclide x y)) xs ++ toDistances xs

euclide :: Coords -> Coords -> Float
euclide (x1, y1, z1) (x2, y2, z2) = sqrt (fromIntegral (square (x1 - x2) + square (y1 - y2) + square (z1 - z2)))

square :: Int -> Int
square n = n * n

parse :: String -> Coords
parse line = (read . unpack . (!! 0) $ parts, read . unpack . (!! 1) $ parts, read . unpack . (!! 2) $ parts)
  where
    parts = splitOn (pack ",") (pack line)

main :: IO ()
main = do readFile "day08.dat" >>= print . solution
