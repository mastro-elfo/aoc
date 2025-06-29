import Data.List (sort)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Coords = (Int, Int)

type Instruction = (Char, Int)

type Segment = (Coords, Coords)

solution :: String -> Int
solution content = (!! 0) . sort . map (manhattan (0, 0)) $ [coords | (s1, s2) <- [(s1, s2) | s1 <- parse . (!! 0) $ lns, s2 <- parse . (!! 1) $ lns, s1 `intersect` s2], coords <- Main.unfold s1, coords `elem` Main.unfold s2, coords /= (0, 0)]
  where
    lns = lines content

unfold :: Segment -> [Coords]
unfold ((sx, sy), (ex, ey))
  | isVertical ((sx, sy), (ex, ey)) = [(sx, y) | y <- [(min sy ey) .. (max sy ey)]]
  | otherwise = [(x, sy) | x <- [(min sx ex) .. (max sx ex)]]

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
parse line = helper [] . map parseInstruction $ splitOn ',' line
  where
    helper [] (instr : xs) = ((0, 0), move (0, 0) instr) : helper [((0, 0), move (0, 0) instr)] xs
    helper segments [] = segments
    helper segments (instr : xs) = segments ++ [(snd . last $ segments, move (snd . last $ segments) instr)] ++ helper [(snd . last $ segments, move (snd . last $ segments) instr)] xs

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

move :: Coords -> Instruction -> Coords
move (x, y) ('R', amount) = (x + amount, y)
move (x, y) ('L', amount) = (x - amount, y)
move (x, y) ('U', amount) = (x, y + amount)
move (x, y) ('D', amount) = (x, y - amount)
move (x, y) (_, amount) = (x, y)

parseInstruction :: String -> Instruction
parseInstruction word = (head word, read . tail $ word)

manhattan :: (Num a) => (a, a) -> (a, a) -> a
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do openFile "day03.dat" ReadMode >>= hGetContents >>= print . solution
