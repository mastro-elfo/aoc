solution :: String -> Int
solution = play 0 0 . map read . lines

play :: Int -> Int -> [Int] -> Int
play step index jumps 
    | index < 0 || index >= length jumps = step
    | otherwise = play (step +1) (index + (!!index) jumps) (take index jumps ++ [(!!index) jumps + (if' ((!!index) jumps < 3) (1) (-1))] ++ drop (index+1) jumps) 

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
