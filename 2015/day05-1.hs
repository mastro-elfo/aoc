import Data.List (isInfixOf)


solution :: String -> Int
solution =  length . filter isNice . lines

isNice :: String -> Bool
isNice xs = all (\f -> f xs) [hasDoubledLetter, hasMoreThanThreeVowels, hasNoNaugtyStrings]

hasMoreThanThreeVowels :: String -> Bool
hasMoreThanThreeVowels = (>=3) . length . filter (`elem` "aeiou")

hasNoNaugtyStrings :: String -> Bool
hasNoNaugtyStrings xs = all (\x -> not (isInfixOf x xs)) ["ab", "cd", "pq", "xy"]

hasDoubledLetter :: String -> Bool
hasDoubledLetter "" = False
hasDoubledLetter (x:"") = False
hasDoubledLetter (x:y:xs)
    | x == y = True
    | otherwise = hasDoubledLetter (y:xs)

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
