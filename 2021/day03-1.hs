import Data.Char (ord)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

solution :: String -> Int
solution content = toInt (mostCommonValue acc) * toInt (leastCommonValue acc)
  where
    acc = countAll . map (map (\x -> ord x - ord '0')) . lines $ content

toInt :: [Int] -> Int
toInt [] = 0
toInt xs = helper 0 (reverse xs)
  where
    helper _ [] = 0
    helper idx (x : xs) = x * power2 idx + helper (idx + 1) xs

power2 :: Int -> Int
power2 0 = 1
power2 n = 2 * power2 (n - 1)

mostCommonValue :: [(Int, Int)] -> [Int]
mostCommonValue [] = []
mostCommonValue ((c0, c1) : cs)
  | c0 > c1 = 0 : mostCommonValue cs
  | otherwise = 1 : mostCommonValue cs

leastCommonValue :: [(Int, Int)] -> [Int]
leastCommonValue [] = []
leastCommonValue ((c0, c1) : cs)
  | c0 < c1 = 0 : leastCommonValue cs
  | otherwise = 1 : leastCommonValue cs

countAll :: [[Int]] -> [(Int, Int)]
countAll xs = foldl countLine ([(0, 0) | _ <- [1 .. (length . (!! 0) $ xs)]]) xs

countLine :: [(Int, Int)] -> [Int] -> [(Int, Int)]
countLine [] _ = []
countLine _ [] = []
countLine ((c0, c1) : cs) (0 : xs) = (c0 + 1, c1) : countLine cs xs
countLine ((c0, c1) : cs) (1 : xs) = (c0, c1 + 1) : countLine cs xs

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

main :: IO ()
main = do openFile "day03.dat" ReadMode >>= hGetContents >>= print . solution
