import Data.List (isInfixOf)


solution :: String -> Int
solution =  length . filter isNice . lines

isNice :: String -> Bool
isNice xs = all (\f -> f xs) [hasDoubledLetter, hasMoreThanThreeVowels, hasNoNaughtyStrings]

hasMoreThanThreeVowels :: String -> Bool
hasMoreThanThreeVowels = (>=3) . length . filter (`elem` "aeiou")

hasNoNaughtyStrings :: String -> Bool
hasNoNaughtyStrings xs = all (\x -> not (isInfixOf x xs)) ["ab", "cd", "pq", "xy"]

hasDoubledLetter :: String -> Bool
hasDoubledLetter (x:y:xs)
    | x == y = True
    | otherwise = hasDoubledLetter (y:xs)
hasDoubledLetter _ = False

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
