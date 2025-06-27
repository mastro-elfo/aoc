import Control.Arrow (ArrowChoice (left, right))
import Distribution.Compat.Lens (over)
import System.IO

type Rectangle = (Int, Int, Int, Int, Int)

solution :: String -> Int
solution content = rId . (!! 0) . filter (\r1 -> all (\r2 -> r1 == r2 || not (overlap r1 r2)) rects) $ rects
  where
    rects = map parse . lines $ content

rId :: Rectangle -> Int
rId (rid, _, _, _, _) = rid

overlap :: Rectangle -> Rectangle -> Bool
overlap r1 r2 = isInside r1 r2 || isInside r2 r1 || isOutside r1 r2 || isOutside r2 r1

isOutside :: Rectangle -> Rectangle -> Bool
isOutside (_, left1, top1, _, bottom1) (_, left2, top2, right2, _) = left2 <= left1 && left1 < right2 && top1 <= top2 && top2 < bottom1

isInside :: Rectangle -> Rectangle -> Bool
isInside (_, left, top, right, bottom) rect = isInternal left top rect || isInternal left bottom rect || isInternal right top rect || isInternal right bottom rect

isInternal :: Int -> Int -> Rectangle -> Bool
isInternal x y (_, left, top, right, bottom) = left <= x && x < right && top <= y && y < bottom

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
