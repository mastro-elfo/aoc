import Data.Map qualified
import Data.Map.Strict (member)

type Paper = (Int, Int)

solution :: String -> Int
solution content = length papers - (length . remove $ papers)
  where
    papers = parse (0, 0) content

remove :: Data.Map.Map Paper Int -> Data.Map.Map Paper Int
remove papers
  | filtered == papers = papers
  | otherwise = remove filtered
  where
    filtered = Data.Map.filter (>= 4) . Data.Map.mapWithKey (\k _ -> length . neighbors papers $ k) $ papers

neighbors :: Data.Map.Map Paper Int -> Paper -> [Paper]
neighbors mp (x, y) = filter (`member` mp) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]

parse :: Paper -> String -> Data.Map.Map Paper Int
parse _ [] = Data.Map.empty
parse (x, y) ('@' : xs) = Data.Map.insert (x, y) 0 (parse (x + 1, y) xs)
parse (x, y) ('\n' : xs) = parse (0, y + 1) xs
parse (x, y) (_ : xs) = parse (x + 1, y) xs

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
