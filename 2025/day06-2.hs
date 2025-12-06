data Operation = SUM | MUL

solution :: String -> Int
solution = sum . map operate . uncurry zip . parse

operate :: ([Int], Operation) -> Int
operate (xs, SUM) = sum xs
operate (xs, MUL) = product xs

group :: [[a]] -> [[[a]]]
group xs = helper [] xs
  where
    helper ys [] = ys
    helper ys ([] : xs) = helper (ys ++ [[]]) xs
    helper [] (x : xs) = helper [[x]] xs
    helper ys (x : xs) = helper (init ys ++ [x : last ys]) xs

strip :: String -> String
strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

transpose :: (Show a) => [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = map head rows : transpose (map tail rows)

parse :: String -> ([[Int]], [Operation])
parse content =
  ( map (map read) . group . map strip . transpose . init . lines $ content,
    parseOperations . last . lines $ content
  )

parseOperations :: String -> [Operation]
parseOperations "" = []
parseOperations ('+' : xs) = SUM : parseOperations xs
parseOperations ('*' : xs) = MUL : parseOperations xs
parseOperations (_ : xs) = parseOperations xs

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
