type Coords = (Int, Int)

solution :: String -> Int
solution content = length . filter (< 10_000) $ theMap
  where
    pivots = map parse . lines $ content
    hlimit = maximum . map fst $ pivots
    vlimit = maximum . map snd $ pivots
    theMap = [sum [manhattan (x, y) pivot | pivot <- pivots] | x <- [0 .. hlimit], y <- [0 .. vlimit]]

manhattan :: Coords -> Coords -> Int
manhattan (c1x, c1y) (c2x, c2y) = abs (c1x - c2x) + abs (c1y - c2y)

parse :: String -> Coords
parse xs =
  ( read . init . head . words $ xs,
    read . last . words $ xs
  )

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
