import Data.Map (Map)
import Data.Map qualified as Map

type Coords = (Int, Int)

type Memo = Map Coords Int

solution :: String -> Int
solution content = evaluate (Map.insert (0, 0) 1 Map.empty) (read content) (generator (0, 0))

evaluate :: Memo -> Int -> [Coords] -> Int
evaluate memo target (x : xs)
  | current >= target = current
  | x == (0, 0) = evaluate memo target xs
  | otherwise = evaluate (Map.insert x current memo) target xs
  where
    current = Map.foldl (+) 0 (neighbors memo x)

neighbors :: Memo -> Coords -> Memo
neighbors memo (x, y) = Map.filterWithKey (\k _ -> k `elem` [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1)]) memo

generator :: Coords -> [Coords]
generator (x, y) = (x, y) : rest 1 (x, y)
  where
    rest :: Integer -> Coords -> [Coords]
    rest side (x, y) = (x + 1, y) : part1 (side + 2) (x + 1, y)

    part1 :: Integer -> Coords -> [Coords]
    part1 side (x, y)
      | toInteger y >= (side `div` 2) = part2 side (x, y)
      | otherwise = (x, y + 1) : part1 side (x, y + 1)

    part2 :: Integer -> Coords -> [Coords]
    part2 side (x, y)
      | toInteger x <= ((-side) `div` 2) + 1 = part3 side (x, y)
      | otherwise = (x - 1, y) : part2 side (x - 1, y)

    part3 :: Integer -> Coords -> [Coords]
    part3 side (x, y)
      | toInteger y <= ((-side) `div` 2 + 1) = part4 side (x, y)
      | otherwise = (x, y - 1) : part3 side (x, y - 1)

    part4 :: Integer -> Coords -> [Coords]
    part4 side (x, y)
      | toInteger x >= (side `div` 2) = rest side (x, y)
      | otherwise = (x + 1, y) : part4 side (x + 1, y)

main :: IO ()
main = do readFile "day03.dat" >>= print . solution
