type Paper = (Int, Int)

solution :: String -> Int
solution content = length papers - (length . remove $ papers)
  where
    papers = parse (0, 0) content

remove :: [Paper] -> [Paper]
remove papers
  | null filtered = papers
  | otherwise = remove . filter (`notElem` filtered) $ papers
  where
    filtered = map fst . filter ((< 4) . snd) $ zip papers (map (length . neighbors papers) papers)

neighbors :: [Paper] -> Paper -> [Paper]
neighbors ps (x, y) = take 4 . filter (`elem` [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]) $ ps

parse :: Paper -> String -> [Paper]
parse _ [] = []
parse (x, y) ('@' : xs) = (x, y) : parse (x + 1, y) xs
parse (x, y) ('\n' : xs) = parse (0, y + 1) xs
parse (x, y) (_ : xs) = parse (x + 1, y) xs

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
