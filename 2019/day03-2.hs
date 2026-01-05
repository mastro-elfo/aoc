import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Distribution.Simple.Utils (safeTail)

data Direction = U | D | L | R

type Coords = (Int, Int)

type Instruction = (Direction, Int)

type Segment = (Coords, Coords)

solution :: String -> Int
solution content =
  minimum . map (\coords -> fromJust (elemIndex coords wire1) + fromJust (elemIndex coords wire2)) $
    [ coords
      | s1 <- w1Seg,
        s2 <- w2Seg,
        s1 `intersect` s2,
        coords <- Main.unfold s1,
        coords `elem` Main.unfold s2,
        coords /= (0, 0)
    ]
  where
    w1Seg = parse . (!! 0) . lines $ content
    w2Seg = parse . (!! 1) . lines $ content
    wire1 = concatMap unfold w1Seg
    wire2 = concatMap unfold w2Seg

unfold :: Segment -> [Coords]
unfold ((sx, sy), (ex, ey))
  | isVertical ((sx, sy), (ex, ey)) = [(sx, y) | y <- [(min sy ey) .. (max sy ey - 1)]]
  | otherwise = [(x, sy) | x <- [(min sx ex) .. (max sx ex - 1)]]

intersect :: Segment -> Segment -> Bool
intersect ((s1x, s1y), (e1x, e1y)) ((s2x, s2y), (e2x, e2y))
  | isVertical ((s1x, s1y), (e1x, e1y)) && isVertical ((s2x, s2y), (e2x, e2y)) =
      (s1y == s2y)
        && ( between s1y s2y e2y
               || between e1y s2y e2y
               || between s2y s1y e1y
               || between e2y s1y e1y
           )
  | not (isVertical ((s1x, s1y), (e1x, e1y))) && isVertical ((s2x, s2y), (e2x, e2y)) = Main.between s1y s2y e2y && Main.between s2x s1x e1x
  | isVertical ((s1x, s1y), (e1x, e1y)) && not (isVertical ((s2x, s2y), (e2x, e2y))) = Main.between s1x s2x e2x && Main.between s2y s1y e1y
  | otherwise =
      (s1x == s2x)
        && ( between s1x s2x e2x
               || between e1x s2x e2x
               || between s2x s1x e1x
               || between e2x s1x e1x
           )

isVertical :: Segment -> Bool
isVertical ((sx, _), (ex, _)) = sx == ex

between :: (Ord a) => a -> a -> a -> Bool
between x start end
  | start < end = start <= x && x <= end
  | otherwise = end <= x && x <= start

parse :: String -> [Segment]
parse = helper (0, 0)
  where
    helper cur "" = []
    helper cur line = (cur, end) : helper end (safeTail . dropWhile (/= ',') $ line)
      where
        instr = parseInstruction . takeWhile (/= ',') $ line
        end = move cur instr

move :: Coords -> Instruction -> Coords
move (x, y) (R, amount) = (x + amount, y)
move (x, y) (L, amount) = (x - amount, y)
move (x, y) (U, amount) = (x, y + amount)
move (x, y) (D, amount) = (x, y - amount)

parseInstruction :: String -> Instruction
parseInstruction ('U' : xs) = (U, read xs)
parseInstruction ('D' : xs) = (D, read xs)
parseInstruction ('L' : xs) = (L, read xs)
parseInstruction ('R' : xs) = (R, read xs)

main :: IO ()
main = do readFile "day03.dat" >>= print . solution
