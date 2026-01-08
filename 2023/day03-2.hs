import Debug.Trace (trace)

type Coord = (Int, Int)

type Component = (Int, [Coord])

solution :: String -> Int
solution content =
  sum
    . map product
    . filter ((== 2) . length)
    . map (\star -> map fst . filter (isNearby star) $ components)
    $ stars
  where
    components = parseComponents content
    stars = parseStars content

isNearby :: Coord -> Component -> Bool
isNearby star (_, cs) = any (`elem` cs) (surroundings star)

surroundings :: Coord -> [Coord]
surroundings (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

parseStars :: String -> [Coord]
parseStars = helper 0 0
  where
    helper _ _ "" = []
    helper _ y ('\n' : cs) = helper 0 (y + 1) cs
    helper x y (c : cs)
      | c == '*' = (x, y) : helper (x + 1) y cs
      | otherwise = helper (x + 1) y cs

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
