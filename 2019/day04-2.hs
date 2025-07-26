import Data.List (group)
import Data.Char (ord)

solution :: String -> Int
solution content = length . filter (increasing . show) . filter (twoAdjacentEqual . show) $ [fst..lst]
    where
        limits = parse content
        fst = head limits
        lst = last limits

twoAdjacentEqual :: String -> Bool
twoAdjacentEqual xs = any (==2) (map length . group $ xs)

increasing :: String -> Bool
increasing xs = all (\(a,b) -> a == b || ord a < ord b) (zip xs (tail xs))

parse :: String -> [Int]
parse = map read . splitOn '-'

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
