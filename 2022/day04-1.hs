type Limits = (Int, Int)
type Pair = (Limits, Limits)

solution :: String -> Int
solution = length . filter isContained . map parse . lines

isContained :: Pair -> Bool
isContained  (first, second) = helper first second || helper second first
    where 
        helper (fs, fe) (ss, se) = ss <= fs && fs <= se && ss <= fe && fe <= se

parse :: String -> Pair
parse xs =  ((firstStart, firstEnd), (secondStart, secondEnd))
    where
        (first: second: _) = splitOn ',' xs
        (firstStart: firstEnd: _) = map read . splitOn '-' $ first
        (secondStart: secondEnd: _) = map read . splitOn '-' $ second

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
main = do readFile "day04.dat" >>= print . solution
