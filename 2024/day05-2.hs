import Data.List (elemIndex, find)
import Data.Maybe (fromJust, isNothing)

type Rule = (Int, Int)

solution :: String -> Int
solution content = sum . map (middle . flip correct rules) . filter (`isInvalid` rules) $ pages
  where
    rules = parseRules . takeWhile (/= "") . lines $ content
    pages = parsePages . tail . dropWhile (/= "") . lines $ content

correct :: [Int] -> [Rule] -> [Int]
correct pages rules
  | isNothing wrong = pages
  | otherwise = correct (swap (fromJust wrong) pages) rules
  where
    wrong = find (isWrong pages) rules

swap :: Rule -> [Int] -> [Int]
swap (first, second) pages
  | isNothing firstIndex = pages
  | isNothing secondIndex = pages
  | otherwise =
      take (fromJust secondIndex) pages
        ++ [first]
        ++ drop (fromJust secondIndex + 1) (take (fromJust firstIndex) pages)
        ++ [second]
        ++ drop (fromJust firstIndex + 1) pages
  where
    firstIndex = elemIndex first pages
    secondIndex = elemIndex second pages

middle :: [Int] -> Int
middle [] = 0
middle [x] = x
middle xs = middle . init . tail $ xs

isWrong :: [Int] -> Rule -> Bool
isWrong pages (first, second)
  | isNothing firstIndex = False
  | isNothing secondIndex = False
  | otherwise = firstIndex > secondIndex
  where
    firstIndex = elemIndex first pages
    secondIndex = elemIndex second pages

isInvalid :: [Int] -> [Rule] -> Bool
isInvalid _ [] = False
isInvalid pages (r : rs) = isWrong pages r || isInvalid pages rs

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
