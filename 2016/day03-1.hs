import System.IO

solution :: String -> Int
solution = length . filter isTriangle . map (map read . words) . lines

isTriangle :: [Int] -> Bool
isTriangle (a : b : c : _)
  | a + b > c && a + c > b && b + c > a = True
  | otherwise = False

main :: IO ()
main = do openFile "day03.dat" ReadMode >>= hGetContents >>= print . solution
