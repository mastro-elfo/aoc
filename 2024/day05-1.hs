import Data.List (findIndex)
import Data.Maybe (isNothing, fromJust)
import Debug.Trace (trace)

type Rule = (Int, Int)

solution :: String -> Int
solution content = sum . map middle . filter (flip isValid rules) $ pages
    where
        rules = parseRules . takeWhile (/= "") . lines $ content
        pages = parsePages . tail . dropWhile (/= "") . lines $ content

middle :: [Int] -> Int
middle [] = 0
middle (x:[]) = x
middle xs = middle . init . tail $ xs

isValid :: [Int] -> [Rule] -> Bool
isValid _ [] = True
isValid pages ((first, second) : rs)
    | isNothing firstIndex = isValid pages rs
    | isNothing secondIndex = isValid pages rs
    | otherwise = firstIndex < secondIndex && isValid pages rs
    where
        firstIndex = findIndex (== first) pages
        secondIndex = findIndex (== second) pages

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
