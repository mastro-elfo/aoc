solution :: String -> Int
solution = helper True 0
  where
    helper :: Bool -> Int -> String -> Int
    helper _ acc "" = acc
    helper _ acc ('d' : 'o' : '(' : ')' : ys) = helper True acc ys
    helper _ acc ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : ys) = helper False acc ys
    helper c acc ('m' : 'u' : 'l' : '(' : ys)
      | not c = helper False acc ys
      | isValidInput ys = helper True (acc + (product . map read . splitOn ',' . takeWhile (/= ')') $ ys)) (dropWhile (/= ')') ys)
      | otherwise = helper c acc ys
    helper c acc (_ : ys) = helper c acc ys

isValidInput :: [Char] -> Bool
isValidInput = all isAllDigits . splitOn ',' . takeWhile (/= ')')

isAllDigits :: [Char] -> Bool
isAllDigits = all (`elem` "1234567890")

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
main = do readFile "day03.dat" >>= print . solution
