import System.IO

solution :: String -> Int
solution = countTrees (0, 0) (3, 1) . lines

countTrees :: (Int, Int) -> (Int, Int) -> [String] -> Int
countTrees (startX, startY) (deltaX, deltaY) treemap
  | startY < length treemap = if' ((!! startX) ((!! startY) treemap) == '#') 1 0 + countTrees ((startX + deltaX) `mod` length ((!! 0) treemap), startY + deltaY) (deltaX, deltaY) treemap
  | otherwise = 0

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

main :: IO ()
main = do openFile "day03.dat" ReadMode >>= hGetContents >>= print . solution
