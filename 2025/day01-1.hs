data Direction = L | R

type Amount = Int

type Instruction = (Direction, Amount)

solution :: String -> Int
solution = snd . foldl rotate (50, 0) . map parse . lines

rotate :: (Int, Amount) -> Instruction -> (Int, Amount)
rotate (ps, zs) (L, am) = (new, zs + oneIfZero new)
  where
    new = norm (ps - am)
rotate (ps, zs) (R, am) = (new, zs + oneIfZero new)
  where
    new = norm (ps + am)

oneIfZero :: Int -> Int
oneIfZero 0 = 1
oneIfZero _ = 0

norm :: Int -> Int
norm n
  | n < 0 = norm (100 + n)
  | n > 99 = norm (n - 100)
  | otherwise = n

parse :: String -> Instruction
parse ('L' : xs) = (L, read xs)
parse ('R' : xs) = (R, read xs)

main :: IO ()
main = do readFile "day01.dat" >>= print . solution
