import Data.Char (isSpace)
import Data.List (dropWhileEnd, elemIndex, isInfixOf)
import Data.Maybe (fromJust)

type Rule = (String, [String])

solution :: String -> Int
solution content = length . filter (isValid rules) $ rules
  where
    rules = map parse . lines $ content

isValid :: [Rule] -> Rule -> Bool
isValid rules rule = ("shiny gold" `elem` contains) || any (isValid rules) (filter (\r -> fst r `elem` contains) rules)
  where
    (name, contains) = rule

parse :: String -> Rule
parse line
  | "no other bags" `isInfixOf` line = (name, [])
  | otherwise = (name, map parseContain (splitOn ',' (unwords (drop (bagsIndex + 2) parts))))
  where
    parts = words line
    bagsIndex = fromJust $ elemIndex "bags" parts
    name = unwords $ take bagsIndex parts

parseContain :: String -> String
parseContain = getTheColor . words

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
