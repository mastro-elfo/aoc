import Data.Char (isSpace)
import Data.List (dropWhileEnd, elemIndex, find, isInfixOf)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Rule = (String, [(String, Int)])

strace :: (Show a) => a -> a
strace a = trace (show a) a

solution :: String -> Int
solution content = count (map parse . lines $ content) "shiny gold"

count :: [Rule] -> String -> Int
count rules name =
  sum
    . map (\(sub, num) -> num * (1 + count rules sub))
    . snd
    . fromJust
    . find (\r -> fst r == name)
    $ rules

parse :: String -> Rule
parse line
  | "no other bags" `isInfixOf` line = (name, [])
  | otherwise = (name, map parseContain (splitOn ',' (unwords (drop (bagsIndex + 2) parts))))
  where
    parts = words line
    bagsIndex = fromJust $ elemIndex "bags" parts
    name = unwords $ take bagsIndex parts

parseContain :: String -> (String, Int)
parseContain line = (getTheColor . words $ line, getTheNumber . words $ line)

getTheNumber :: [String] -> Int
getTheNumber (x : xs)
  | all (`elem` "1234567890") x = read x
  | otherwise = getTheNumber xs

getTheColor :: [String] -> String
getTheColor [] = ""
getTheColor (x : xs)
  | all (`elem` "1234567890") x = getTheColor xs
  | x `elem` ["bag", "bags", "bag.", "bags."] = getTheColor xs
  | otherwise = dropWhileEnd isSpace . dropWhile isSpace . unwords $ [x, getTheColor xs]

splitOn :: Char -> String -> [String]
splitOn c xs = helper [""] c xs
  where
    helper o c "" = o
    helper o c (x : "")
      | x == c = o
      | otherwise = init o ++ [last o ++ [x]]
    helper o c (x : xs)
      | x == c = helper (o ++ [""]) c xs
      | otherwise = helper (init o ++ [last o ++ [x]]) c xs

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
