data Direction = L | R deriving (Eq, Show)

type Amount = Int

type Instruction = (Direction, Amount)

solution :: String -> Int
solution = rotate 0 50 . map parse . lines

rotate :: Int -> Int -> [Instruction] -> Int
rotate c _ [] = c
rotate c ps ((_, 0) : xs) = rotate c ps xs
rotate c 0 ((L, n) : xs) = rotate c 99 ((L, n - 1) : xs)
rotate c 1 ((L, n) : xs) = rotate (c + 1) 0 ((L, n - 1) : xs)
rotate c ps ((L, n) : xs) = rotate c (ps - 1) ((L, n - 1) : xs)
rotate c 99 ((R, n) : xs) = rotate (c + 1) 0 ((R, n - 1) : xs)
rotate c ps ((R, n) : xs) = rotate c (ps + 1) ((R, n - 1) : xs)

parse :: String -> Instruction
parse ('L' : xs) = (L, read xs)
parse ('R' : xs) = (R, read xs)

main :: IO ()
main = do readFile "day01.dat" >>= print . solution
