import Data.Function (on)
import Data.List (find, sort, sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Ord (Down (..), comparing)
import Data.Set (fromList, toList)
import Data.Text (pack, splitOn, unpack)
import Distribution.Utils.Generic (trdOf3)

type Coords = (Int, Int, Int)

type Distances = (Coords, Coords, Float)

type Circuit = [Coords]

solution :: String -> Int
solution = product . take 3 . sortBy (comparing Down) . map length . connect . map (\(a, b, _) -> [a, b]) . take 1000 . sortBy (compare `on` trdOf3) . toDistances . map parse . lines

connect :: [Circuit] -> [Circuit]
connect [] = []
connect (cs : css)
  | isJust other = connect (unique (cs ++ fromJust other) : filter (/= fromJust other) css)
  | otherwise = cs : connect css
  where
    other = find (\os -> any (`elem` os) cs) css

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
