import Data.List (isPrefixOf)
import Distribution.Simple.Utils (safeTail)

type Range = (Int, Int)

solution :: String -> Int
solution = sum . concatMap (filter isInvalidId . unwrap . parseRange) . parse

unwrap :: Range -> [Int]
unwrap (start, end) = [start .. end]

isInvalidPattern :: String -> String -> Bool
isInvalidPattern _ "" = True
isInvalidPattern ps xs = ps `isPrefixOf` xs && isInvalidPattern ps (drop (length ps) xs)

isInvalidId :: Int -> Bool
isInvalidId n = any (\l -> isInvalidPattern (take l digits) digits) [1 .. half]
  where
    digits = show n
    half = length digits `div` 2

parseRange :: String -> Range
parseRange line = (read . takeWhile (/= '-') $ line, read . tail . dropWhile (/= '-') $ line)

parse :: String -> [String]
parse "" = []
parse content = takeWhile (/= ',') content : (parse . safeTail . dropWhile (/= ',') $ content)

main :: IO ()
main = do readFile "day02.dat" >>= print . solution
