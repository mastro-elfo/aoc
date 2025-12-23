import Data.ByteString.Char8 (pack)
import Distribution.Utils.MD5 (md5, showMD5)

solution :: String -> String
solution = map (!! 5) . take 8 . filter isValid . hash

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
