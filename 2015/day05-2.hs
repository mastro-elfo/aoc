import Data.List (isInfixOf)

solution :: String -> Int
solution = length . filter isNice . lines

isNice :: String -> Bool
isNice xs = all (\f -> f xs) [hasRepeatingDouble, hasEfeRule]

hasEfeRule :: String -> Bool
hasEfeRule (x:y:z:xs)
    | x == z = True
    | otherwise = hasEfeRule (y:z:xs)
hasEfeRule _ = False

hasRepeatingDouble :: String -> Bool
hasRepeatingDouble (x:y:xs)
    | isInfixOf (x:y:[]) xs = True
    | otherwise = hasRepeatingDouble (y:xs)
hasRepeatingDouble _ = False

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
