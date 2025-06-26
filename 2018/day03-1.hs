module Aoc where

import Data.Map (Map, empty, filter, insert, lookup, size)
import Data.Maybe (isNothing)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Rectangle = (Int, Int, Int, Int, Int)

type Coords = (Int, Int)

type Fabric = Map Coords Int

solution :: String -> Int
solution = size . Data.Map.filter (== 0) . foldl draw empty . map parse . lines

draw :: Fabric -> Rectangle -> Fabric
draw fabric rect = foldl (\f (x, y) -> drawAt rid f x y) fabric $ [(x, y) | x <- [left .. (right - 1)], y <- [top .. (bottom - 1)]]
  where
    (rid, left, top, right, bottom) = rect

drawAt :: Int -> Fabric -> Int -> Int -> Fabric
drawAt rid fabric x y = insert (x, y) (if isNothing . Data.Map.lookup (x, y) $ fabric then rid else 0) fabric

parse :: [Char] -> Rectangle
parse xs = (rid, left, top, right, bottom)
  where
    parts = words xs
    rid = read . tail . (!! 0) $ parts
    left = read . (!! 0) . splitOn ',' . (!! 2) $ parts
    top = read . init . (!! 1) . splitOn ',' . (!! 2) $ parts
    right = left + (read . (!! 0) . splitOn 'x' . (!! 3) $ parts)
    bottom = top + (read . (!! 1) . splitOn 'x' . (!! 3) $ parts)

splitOn :: Char -> String -> [String]
splitOn c xs = helper [""] c xs
  where
    helper o c "" = o
    helper o c (x : "")
      | x == c = o
      | otherwise = init o ++ [last o ++ [x]]
    helper o c (x : xs)
      | x == c = helper (o ++ [""]) c xs
      | otherwise = helper (init o ++ [last o ++ [x]]) c xs

main :: IO ()
main = do openFile "day03.dat" ReadMode >>= hGetContents >>= print . solution
