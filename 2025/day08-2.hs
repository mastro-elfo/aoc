{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Set (fromList, toList)
import Data.Text (pack, splitOn, unpack)
import Distribution.Utils.Generic (trdOf3)

type Coords = (Int, Int, Int)

type Distances = (Coords, Coords, Float)

type Circuit = [Coords]

type Connection = (Coords, Coords)

solution :: String -> Int
solution content =
  multiplyX
    . connect (length boxes) []
    . map (\(a, b, _) -> (a, b))
    . sortBy (compare `on` trdOf3)
    . toDistances
    $ boxes
  where
    boxes = map parse . lines $ content

connect :: Int -> [Circuit] -> [Connection] -> Connection
connect n cs [d] = d
connect n cs (d : ds)
  | (length . head $ newconfig) == n = d
  | otherwise = connect n newconfig ds
  where
    newconfig = merge cs d

merge :: [Circuit] -> Connection -> [Circuit]
merge cs (box1, box2)
  | isJust c1 && isJust c2 && justC1 == justC2 = cs
  | isJust c1 && isJust c2 = unique (justC1 ++ justC2) : filter (\c -> c /= justC1 || c /= justC2) cs
  | isJust c1 = (box2 : justC1) : filter (/= justC1) cs
  | isJust c2 = (box1 : justC2) : filter (/= justC2) cs
  | otherwise = [box1, box2] : cs
  where
    c1 = find (\c -> box1 `elem` c) cs
    c2 = find (\c -> box2 `elem` c) cs
    justC1 = fromJust c1
    justC2 = fromJust c2

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
