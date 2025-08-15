solution :: String -> Int
solution content = sum . map (countAt matrix) $ [(x, y) | x <- [0 .. length matrix -1], y <- [0 .. hlength matrix -1], matrix !! x !! y == 'X']
    where
        matrix = lines content

countAt :: [String] -> (Int, Int) -> Int
countAt matrix xcoord = length . filter id $ [
    isHorizontalForward matrix xcoord,
    isHorizontalBackward matrix xcoord,
    isVerticalDownward matrix xcoord,
    isVerticalUpward matrix xcoord,
    isDiagonalTLBR matrix xcoord,
    isDiagonalBRTL matrix xcoord,
    isDiagonalBLTR matrix xcoord,
    isDiagonalTRBL matrix xcoord
    ]

isHorizontalForward :: [String] -> (Int, Int) -> Bool
isHorizontalForward matrix (row, col) = (col < (hlength matrix -3) )
    && "MAS" == extract matrix (zip (need 3 row) (next 3 col))

isHorizontalBackward :: [String] -> (Int, Int) -> Bool
isHorizontalBackward matrix (row, col) = (col > 2)
    && "MAS" == extract matrix (zip (need 3 row) (reverse (prev 3 col)))

isVerticalDownward :: [String] -> (Int, Int) -> Bool
isVerticalDownward matrix (row, col) = (row < (length matrix) -3)
    && "MAS" == extract matrix (zip (next 3 row) (need 3 col))

isVerticalUpward :: [String] -> (Int, Int) -> Bool
isVerticalUpward matrix (row, col) = (row > 2)
    && "MAS" == extract matrix (zip (reverse (prev 3 row)) (need 3 col))

isDiagonalTLBR :: [String] -> (Int, Int) -> Bool
isDiagonalTLBR matrix (row, col) = (row < (length matrix) -3) && (col < (hlength matrix) -3)
    && "MAS" == extract matrix (zip (next 3 row) (next 3 col))

isDiagonalBRTL :: [String] -> (Int, Int) -> Bool
isDiagonalBRTL matrix (row, col) = (row > 2) && (col > 2)
    && "MAS" == extract matrix (zip (reverse (prev 3 row)) (reverse (prev 3 col)))

isDiagonalBLTR :: [String] -> (Int, Int) -> Bool
isDiagonalBLTR matrix (row, col) = (row > 2) && (col < (hlength matrix -3))
    && "MAS" == extract matrix (zip (reverse (prev 3 row)) (next 3 col))

isDiagonalTRBL :: [String] -> (Int, Int) -> Bool
isDiagonalTRBL matrix (row, col) = (row < (length matrix) -3) && (col > 2)
    && "MAS" == extract matrix (zip (next 3 row) (reverse (prev 3 col)))

extract :: [[a]] -> [(Int, Int)] -> [a]
extract mx = map (\(r, c) -> mx !! r !! c)

need :: Int -> a -> [a]
need n a = take n (repeat a)

hlength :: [[a]] -> Int
hlength = length . head

next :: Int -> Int -> [Int]
next n x = [x+1 .. x+n]

prev :: Int -> Int -> [Int]
prev n x = [x-n .. x-1]

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
