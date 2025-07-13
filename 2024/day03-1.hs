import Data.Char (ord)

solution :: String -> Int
solution = helper 0
  where
    helper :: Int -> String -> Int
    helper acc "" = acc
    helper acc ('m' : 'u' : 'l' : '(' : ys)
      | isValidInput ys = helper (acc + (product . map read . splitOn ',' . takeWhile (/= ')') $ ys)) (dropWhile (/= ')') ys)
      | otherwise = helper acc ys
    helper acc (_ : ys) = helper acc ys

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
