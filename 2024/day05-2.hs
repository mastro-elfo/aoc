import Data.List (find, findIndex)
import Data.Maybe (isNothing, fromJust)
import Debug.Trace (trace)

type Rule = (Int, Int)

solution :: String -> Int
solution content = sum . map middle . map (flip correct rules) . filter (flip isInvalid rules) $ pages
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
    | otherwise = take (fromJust secondIndex) pages 
        ++ [first] 
        ++ drop (fromJust secondIndex +1) (take (fromJust firstIndex) pages)
        ++ [second] 
        ++ drop (fromJust firstIndex +1) pages
    where
        firstIndex = findIndex (== first) pages
        secondIndex = findIndex (== second) pages

middle :: [Int] -> Int
middle [] = 0
middle (x:[]) = x
middle xs = middle . init . tail $ xs

isWrong :: [Int] -> Rule -> Bool
isWrong pages (first, second)
    | isNothing firstIndex = False
    | isNothing secondIndex = False
    | otherwise = firstIndex > secondIndex
    where
        firstIndex = findIndex (== first) pages
        secondIndex = findIndex (== second) pages

isInvalid :: [Int] -> [Rule] -> Bool
isInvalid _ [] = False
isInvalid pages (r : rs) = isWrong pages r || isInvalid pages rs

parseRules :: [String] -> [Rule]
parseRules [] = []
parseRules (x:xs) = (
    read . takeWhile (/= '|') $ x,
    read . tail . dropWhile (/= '|') $ x
    ) : parseRules xs

parsePages :: [String] -> [[Int]]
parsePages [] = []
parsePages (x:xs) = parsePageGroup x : parsePages xs

parsePageGroup :: String -> [Int]
parsePageGroup "" = []
parsePageGroup xs = (read . takeWhile (/= ',') $ xs) : (parsePageGroup . safeTail . dropWhile (/= ',') $ xs)

safeTail :: [a] -> [a]
safeTail xs
    | length xs == 0 = []
    | otherwise = tail xs

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
