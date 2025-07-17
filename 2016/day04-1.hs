import Data.Function (on)
import Data.List (sortBy)
import Distribution.Utils.Generic (sndOf3)

type Room = (String, Int, String)

solution :: String -> Int
solution = sum . map sndOf3 . filter (\(name, _, chs) -> checksum name == chs) . map parse . lines

third :: (a, b, c) -> c
third (_, _, a) = a

checksum :: String -> String
checksum = map snd . take 5 . sortBy (flip compare `on` fst) . sortBy (compare `on` snd) . helper []
  where
    helper :: [(Int, Char)] -> String -> [(Int, Char)]
    helper count [] = count
    helper count ('-' : ys) = helper count ys
    helper count (' ' : ys) = helper count ys
    helper count (y : ys)
      | y `elem` map snd count = helper ([(c + 1, z) | (c, z) <- count, z == y] ++ filter ((/= y) . snd) count) ys
      | otherwise = helper ((1, y) : count) ys

parse :: String -> Room
parse xs = (unwords . init . init $ parts, read . head . tail . reverse $ parts, last parts)
  where
    parts = splitOn "-[]" xs

splitOn :: String -> String -> [String]
splitOn cs xs = helper [""] cs xs
  where
    helper o c "" = o
    helper o c (x : "")
      | x `elem` cs = o
      | otherwise = init o ++ [last o ++ [x]]
    helper o c (x : xs)
      | x `elem` cs = helper (o ++ [""]) cs xs
      | otherwise = helper (init o ++ [last o ++ [x]]) cs xs

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
