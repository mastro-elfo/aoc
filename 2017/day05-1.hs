solution :: String -> Int
solution = play 0 0 . map read . lines

play :: Int -> Int -> [Int] -> Int
play step index jumps 
    | index < 0 || index >= length jumps = step
    | otherwise = play (step +1) (index + (!!index) jumps) (take index jumps ++ [(!!index) jumps + 1] ++ drop (index+1) jumps) 

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
