import Data.List (elemIndex)
import Data.Maybe (isNothing)

type Rule = (Int, Int)

solution :: String -> Int
solution content = sum . map middle . filter (`isValid` rules) $ pages
  where
    rules = parseRules . takeWhile (/= "") . lines $ content
    pages = parsePages . tail . dropWhile (/= "") . lines $ content

middle :: [Int] -> Int
middle [] = 0
middle [x] = x
middle xs = middle . init . tail $ xs

isValid :: [Int] -> [Rule] -> Bool
isValid _ [] = True
isValid pages ((first, second) : rs)
  | isNothing firstIndex = isValid pages rs
  | isNothing secondIndex = isValid pages rs
  | otherwise = firstIndex < secondIndex && isValid pages rs
  where
    firstIndex = elemIndex first pages
    secondIndex = elemIndex second pages

parseRules :: [String] -> [Rule]
parseRules =
  map
    ( \x ->
        ( read . takeWhile (/= '|') $ x,
          read . tail . dropWhile (/= '|') $ x
        )
    )

parsePages :: [String] -> [[Int]]
parsePages = map parsePageGroup

parsePageGroup :: String -> [Int]
parsePageGroup "" = []
parsePageGroup xs = (read . takeWhile (/= ',') $ xs) : (parsePageGroup . safeTail . dropWhile (/= ',') $ xs)

safeTail :: [a] -> [a]
safeTail xs
  | null xs = []
  | otherwise = tail xs

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
