data Operation = SUM | MUL

solution :: String -> Int
solution = sum . map operate . uncurry zip . parse

operate :: ([Int], Operation) -> Int
operate (xs, SUM) = sum xs
operate (xs, MUL) = product xs

transpose :: [[a]] -> [[a]]
transpose rows = map head rows : transpose (map tail rows)

parse :: String -> ([[Int]], [Operation])
parse content =
  ( transpose . map parseNumbers . init . lines $ content,
    parseOperations . last . lines $ content
  )

parseNumbers :: String -> [Int]
parseNumbers = map read . words

parseOperations :: String -> [Operation]
parseOperations "" = []
parseOperations ('+' : xs) = SUM : parseOperations xs
parseOperations ('*' : xs) = MUL : parseOperations xs
parseOperations (_ : xs) = parseOperations xs

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
