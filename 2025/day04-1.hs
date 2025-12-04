import Data.Map qualified
import Data.Map.Strict (member)

type Paper = (Int, Int)

solution :: String -> Int
solution content = length . Data.Map.keys . Data.Map.filter (< 4) . Data.Map.mapWithKey (\k _ -> length . neighbors papers $ k) $ papers
  where
    papers = parse (0, 0) content

neighbors :: Data.Map.Map Paper Int -> Paper -> [Paper]
neighbors mp (x, y) = Prelude.filter (`member` mp) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]

parse :: Paper -> String -> Data.Map.Map Paper Int
parse _ [] = Data.Map.empty
parse (x, y) ('@' : xs) = Data.Map.insert (x, y) 0 (parse (x + 1, y) xs)
parse (x, y) ('\n' : xs) = parse (0, y + 1) xs
parse (x, y) (_ : xs) = parse (x + 1, y) xs

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
