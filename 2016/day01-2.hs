import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.Set (fromList, toList)

data Direction = N | S | W | E
  deriving (Show)

data Turn = L | R
  deriving (Show)

type Position = (Int, Int, Direction)

type Instruction = (Turn, Int)

type Coordinate = (Int, Int)

solution :: String -> Int
solution = manhattan (0, 0) . process [] (0, 0, N) . map (parse . init) . words

process :: [Coordinate] -> Position -> [Instruction] -> Coordinate
process _ _ [] = (0, 0)
process visited current (i : is)
  | isJust twice = fromJust twice
  | otherwise = process (toList . fromList $ (visited ++ newVisitedCoord)) newCurrent is
  where
    newVisited = move i current
    newVisitedCoord = map toCoord newVisited
    newCurrent = last newVisited
    twice = find (`elem` visited) newVisitedCoord

toCoord :: Position -> Coordinate
toCoord (x, y, _) = (x, y)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

move :: Instruction -> Position -> [Position]
move instruction = walk instruction . turn instruction

walk :: Instruction -> Position -> [Position]
walk (_, steps) (x, y, N) = [(x, y + s, N) | s <- [1 .. steps]]
walk (_, steps) (x, y, S) = [(x, y - s, S) | s <- [1 .. steps]]
walk (_, steps) (x, y, E) = [(x + s, y, E) | s <- [1 .. steps]]
walk (_, steps) (x, y, W) = [(x - s, y, W) | s <- [1 .. steps]]

turn :: Instruction -> Position -> Position
turn (L, _) (x, y, E) = (x, y, N)
turn (R, _) (x, y, E) = (x, y, S)
turn (L, _) (x, y, W) = (x, y, S)
turn (R, _) (x, y, W) = (x, y, N)
turn (L, _) (x, y, N) = (x, y, W)
turn (R, _) (x, y, N) = (x, y, E)
turn (L, _) (x, y, S) = (x, y, E)
turn (R, _) (x, y, S) = (x, y, W)

parse :: String -> Instruction
parse ('L' : xs) = (L, read xs)
parse ('R' : xs) = (R, read xs)

main :: IO ()
main = do readFile "day01.dat" >>= print . solution
