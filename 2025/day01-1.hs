data Direction = L | R deriving (Eq, Show)

type Amount = Int

type Instruction = (Direction, Amount)

solution :: String -> Int
solution = length . filter (== 0) . foldl rotate [50] . map parse . lines

rotate :: [Int] -> Instruction -> [Int]
rotate ps (L, am) = norm (head ps - am) : ps
rotate ps (R, am) = norm (head ps + am) : ps

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
