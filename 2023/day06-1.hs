import Debug.Trace (trace)

solution :: String -> Int
solution = product . map winning . parse

winning :: (Int, Int) -> Int
winning (t, d) = floor (halfT + delta) - ceiling (halfT - delta) + 1 - if' 2 0 (isInt intersect)
  where
    delta = (sqrt . fromIntegral) (t * t - 4 * d) / 2
    halfT = fromIntegral t / 2
    intersect = halfT + delta

parse :: String -> [(Int, Int)]
parse content = zip times dists
  where
    lns = lines content
    times = map read . tail . words . head $ lns
    dists = map read . tail . words . last $ lns

if' :: a -> a -> Bool -> a
if' a _ True = a
if' _ a False = a

isInt :: (RealFrac a) => a -> Bool
isInt x = ceiling x == floor x

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
