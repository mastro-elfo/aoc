import Data.ByteString.Char8 (pack)
import Debug.Trace (trace)
import Distribution.Utils.MD5 (md5, showMD5)

solution :: String -> String
solution = pwd "        " . filter isValid . hash

pwd :: String -> [String] -> String
pwd cur (x : xs)
  | ' ' `notElem` cur = cur
  | position > 7 = pwd cur xs
  | ' ' /= (!! position) cur = pwd cur xs
  | otherwise = pwd (take position cur ++ [(!! 6) x] ++ drop (position + 1) cur) xs
  where
    position = index ((!! 5) x)

index :: (Num a) => Char -> a
index '0' = 0
index '1' = 1
index '2' = 2
index '3' = 3
index '4' = 4
index '5' = 5
index '6' = 6
index '7' = 7
index _ = 8

hash :: String -> [String]
hash = helper 0
  where
    helper :: Int -> String -> [String]
    helper n xs = (showMD5 . md5 . pack $ (xs ++ show n)) : helper (n + 1) xs

isValid :: String -> Bool
isValid ('0' : '0' : '0' : '0' : '0' : _) = True
isValid _ = False

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
