import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf)
import Distribution.Utils.MD5 (md5)

solution :: String -> Int
solution = helper 1
  where
    helper index content
      | "000000" `isPrefixOf` hashed = index
      | otherwise = helper (index + 1) content
      where
        hashed = show . md5 . pack $ (content ++ show index)

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
