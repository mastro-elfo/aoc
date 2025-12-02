import Distribution.Simple.Utils (safeTail)

type Range = (Int, Int)

solution :: String -> Int
solution = sum . concatMap (filter isInvalidId . unwrap . parseRange) . parse

unwrap :: Range -> [Int]
unwrap (start, end) = [start .. end]

isInvalidId :: Int -> Bool
isInvalidId n = left == right
  where
    digits = show n
    half = length digits `div` 2
    left = take half digits
    right = drop half digits

parseRange :: String -> Range
parseRange line = (read . takeWhile (/= '-') $ line, read . tail . dropWhile (/= '-') $ line)

parse :: String -> [String]
parse "" = []
parse content = takeWhile (/= ',') content : (parse . safeTail . dropWhile (/= ',') $ content)

main :: IO ()
main = do readFile "day02.dat" >>= print . solution
