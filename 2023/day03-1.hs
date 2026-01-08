type Coord = (Int, Int)

type Component = (Int, [Coord])

solution :: String -> Int
solution content =
  sum
    . map fst
    . filter (\(_, coords) -> (any (`elem` symbols) . surroundings) coords)
    $ components
  where
    components = parseComponents content
    symbols = parseSymbols content

surroundings :: [Coord] -> [Coord]
surroundings [] = []
surroundings ((x, y) : xs) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]
    ++ surroundings xs

parseSymbols :: String -> [Coord]
parseSymbols = helper 0 0
  where
    helper _ _ "" = []
    helper _ y ('\n' : cs) = helper 0 (y + 1) cs
    helper x y (c : cs)
      | c `elem` ".1234567890" = helper (x + 1) y cs
      | otherwise = (x, y) : helper (x + 1) y cs

parseComponents :: String -> [Component]
parseComponents = helper 0 0 (0, [])
  where
    helper _ _ _ "" = []
    helper _ y (_, []) ('\n' : cs) = helper 0 (y + 1) (0, []) cs
    helper _ y cmp ('\n' : cs) = cmp : helper 0 (y + 1) (0, []) cs
    helper x y (_, []) (c : cs)
      | c `elem` "1234567890" = helper (x + 1) y (read (c : ""), [(x, y)]) cs
      | otherwise = helper (x + 1) y (0, []) cs
    helper x y (v, w) (c : cs)
      | c `elem` "1234567890" = helper (x + 1) y (v * 10 + read (c : ""), (x, y) : w) cs
      | otherwise = (v, w) : helper (x + 1) y (0, []) cs

main :: IO ()
main = do readFile "day03.dat" >>= print . solution
