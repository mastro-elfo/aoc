solution :: String -> Int
solution content = length . filter (check matrix) $ [(x, y) | x <- [0 .. length matrix -1], y <- [0 .. hlength matrix -1], matrix !! x !! y == 'A']
    where matrix = lines content

check :: [String] -> (Int, Int) -> Bool
check matrix (row, col)
    | row == 0 || col == 0 || row == length matrix -1 || col == hlength matrix -1 = False
    | otherwise = any (\f -> f matrix (row, col)) [top, bottom, left, right]

top :: [String] -> (Int, Int) -> Bool
top matrix (row, col) = 
    matrix !! (row-1) !! (col-1) == 'M' &&
    matrix !! (row-1) !! (col+1) == 'M' &&
    matrix !! (row+1) !! (col-1) == 'S' &&
    matrix !! (row+1) !! (col+1) == 'S'

bottom :: [String] -> (Int, Int) -> Bool
bottom matrix (row, col) = 
    matrix !! (row+1) !! (col-1) == 'M' &&
    matrix !! (row+1) !! (col+1) == 'M' &&
    matrix !! (row-1) !! (col-1) == 'S' &&
    matrix !! (row-1) !! (col+1) == 'S'

left :: [String] -> (Int, Int) -> Bool
left matrix (row, col) = 
    matrix !! (row-1) !! (col-1) == 'M' &&
    matrix !! (row+1) !! (col-1) == 'M' &&
    matrix !! (row-1) !! (col+1) == 'S' &&
    matrix !! (row+1) !! (col+1) == 'S'

right :: [String] -> (Int, Int) -> Bool
right matrix (row, col) = 
    matrix !! (row-1) !! (col+1) == 'M' &&
    matrix !! (row+1) !! (col+1) == 'M' &&
    matrix !! (row-1) !! (col-1) == 'S' &&
    matrix !! (row+1) !! (col-1) == 'S'


hlength :: [[a]] -> Int
hlength = length . head

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
