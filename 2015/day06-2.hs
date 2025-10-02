data Action = ON | OFF | TOGGLE

instance Show Action where
  show :: Action -> String
  show ON = "ON"
  show OFF = "OFF"
  show TOGGLE = "TOGGLE"

type Instruction = (Action, Int, Int, Int, Int)

type Light = (Int, Int, Int)

type Lights = [Light]

solution :: String -> Int
solution content = sum . map intensity $ helper lights instructions
  where
    lights = [(x, y, 0) | x <- [0 .. 999], y <- [0 .. 999]]
    instructions = map parse . lines $ content
    helper lights [] = lights
    helper lights (i : ins) = helper (act i lights) ins

intensity :: Light -> Int
intensity (_, _, t) = t

act :: Instruction -> Lights -> Lights
act instruction = map (actAt instruction)

actAt :: Instruction -> Light -> Light
actAt (ON, sx, sy, ex, ey) (x, y, t)
  | between sx ex x && between sy ey y = (x, y, t + 1)
  | otherwise = (x, y, t)
actAt (OFF, sx, sy, ex, ey) (x, y, t)
  | between sx ex x && between sy ey y = (x, y, max 0 (t - 1))
  | otherwise = (x, y, t)
actAt (TOGGLE, sx, sy, ex, ey) (x, y, t)
  | between sx ex x && between sy ey y = (x, y, t + 2)
  | otherwise = (x, y, t)

between :: (Ord a) => a -> a -> a -> Bool
between left right value = left <= value && value <= right

parse :: String -> Instruction
parse ('t' : 'u' : 'r' : 'n' : ' ' : 'o' : 'n' : xs) = (ON, sx, sy, ex, ey)
  where
    (sx, sy, ex, ey) = parseCoords xs
parse ('t' : 'u' : 'r' : 'n' : ' ' : 'o' : 'f' : 'f' : xs) = (OFF, sx, sy, ex, ey)
  where
    (sx, sy, ex, ey) = parseCoords xs
parse ('t' : 'o' : 'g' : 'g' : 'l' : 'e' : xs) = (TOGGLE, sx, sy, ex, ey)
  where
    (sx, sy, ex, ey) = parseCoords xs

parseCoords :: String -> (Int, Int, Int, Int)
parseCoords xs = (sx, sy, ex, ey)
  where
    sx = read . takeWhile (/= ',') $ xs
    sy = read . takeWhile (/= ' ') . tail . dropWhile (/= ',') $ xs
    ex = read . takeWhile (/= ',') . last . words $ xs
    ey = read . takeWhile (/= ' ') . tail . dropWhile (/= ',') . last . words $ xs

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
