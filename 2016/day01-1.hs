import System.IO

data Direction = N | S | W | E

data Turn = L | R

type Position = (Int, Int, Direction)

type Instruction = (Turn, Int)

solution :: String -> Int
solution content = distance (helper (0, 0, N) [parseOne line | line <- words content])
  where
    helper current [] = current
    helper current (x : xs) = helper (move current x) xs

distance :: Position -> Int
distance (x, y, _) = abs x + abs y

parseOne :: String -> Instruction
parseOne (x : xs) = (toTurn x, read [d | d <- xs, d `elem` "1234567890"] :: Int)
  where
    toTurn 'L' = L
    toTurn 'R' = R

move :: Position -> Instruction -> Position
move current instruction = walk (turn current instruction) instruction

walk :: Position -> Instruction -> Position
walk (x, y, N) (_, b) = (x, y + b, N)
walk (x, y, S) (_, b) = (x, y - b, S)
walk (x, y, E) (_, b) = (x + b, y, E)
walk (x, y, W) (_, b) = (x - b, y, W)

turn :: Position -> Instruction -> Position
turn (x, y, N) (L, _) = (x, y, W)
turn (x, y, N) (R, _) = (x, y, E)
turn (x, y, S) (L, _) = (x, y, E)
turn (x, y, S) (R, _) = (x, y, W)
turn (x, y, W) (L, _) = (x, y, S)
turn (x, y, W) (R, _) = (x, y, N)
turn (x, y, E) (L, _) = (x, y, N)
turn (x, y, E) (R, _) = (x, y, S)

main :: IO ()
main = do readFile "day01.dat" >>= print . solution
