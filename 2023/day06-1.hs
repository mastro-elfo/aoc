import Debug.Trace (trace)

solution :: String -> Int
solution = product . map (uncurry winning) . parse

winning :: Int -> Int -> Int
winning t d = length . filter id . map (\x -> d < x * (t - x)) $ [0 .. t]

parse :: String -> [(Int, Int)]
parse content = zip times dists
  where
    lns = lines content
    times = map read . tail . words . head $ lns
    dists = map read . tail . words . last $ lns

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
