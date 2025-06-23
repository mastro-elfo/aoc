import Distribution.Simple.Program.HcPkg (list)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

solution :: String -> Int
solution = helper . map read . words
  where
    helper xs = search (oneInThree 0 xs) + search (oneInThree 1 xs) + search (oneInThree 2 xs)

search :: (Ord a1, Num a2, Num a1) => [a1] -> a2
search [] = 0
search (x : y : z : xs)
  | isTriangle (x, y, z) = 1 + search xs
  | otherwise = search xs

oneInThree :: Int -> [a] -> [a]
oneInThree offset list = helper (drop offset list)
  where
    helper (x : _ : _ : xs) = x : helper xs
    helper (x : _ : xs) = x : helper xs
    helper (x : xs) = x : helper xs
    helper [] = []

isTriangle :: (Ord a, Num a) => (a, a, a) -> Bool
isTriangle (a, b, c)
  | a + b > c && a + c > b && b + c > a = True
  | otherwise = False

main :: IO ()
main = do openFile "day03.dat" ReadMode >>= hGetContents >>= print . solution
