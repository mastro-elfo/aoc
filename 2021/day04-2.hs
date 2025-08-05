
type Cell = (Int, Bool)
type Board = [[Cell]]

solution :: String -> Int
solution content = wn * (sum . map fst . filter (not . snd) . foldl (++) [] $ wb)
    where
        result = play (parseBoards . drop 2 . lines $ content) (map read . splitOn ',' . head . lines $ content)
        wb = fst result
        wn = snd result

isWinningLine :: [Cell] -> Bool
isWinningLine = all (\(_, check) -> check)

isWinner :: Board -> Bool
isWinner board = any isWinningLine board || any isWinningLine (transpose board)

transpose :: [[a]] -> [[a]]
transpose board = [ [((!! row) . (!! col) $ board) | col <- [0..size]] | row <- [0..size]]
    where
        size = length board -1

winner :: [Board] -> Board
winner [] = []
winner (x:xs) 
    | isWinner x = x
    | otherwise = winner xs

play :: [Board] -> [Int] -> (Board, Int)
play _ [] = ([], 0)
play bs ns = helper ([], 0) bs ns
    where
        helper (wb, wn) bs [] = (wb, wn)
        helper (wb, wn) bs (n:ns)
            | length winningBoard > 0 = helper (winningBoard, n) stillNotWinning ns
            | otherwise = helper (wb, wn) stillNotWinning ns
            where
                current = [
                    [[(num, check || num == n) | (num, check) <- row] | row <- board] | board <- bs]
                winningBoard = winner current
                stillNotWinning = filter (not . isWinner) current

parseBoards :: [String] -> [Board]
parseBoards xs = helper [[]] xs
    where
        helper :: [Board] -> [String] -> [Board]
        helper boards [] = boards
        helper boards ("":xs) = helper (boards ++ [[]]) xs
        helper boards (x:xs) = helper ((init boards) ++ [last boards ++ [[((read y)::Int, False) | y <- words x]]]) xs

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

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
