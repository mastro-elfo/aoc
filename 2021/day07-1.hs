import Data.Text (pack, splitOn, unpack)

solution :: String -> Int
solution content = minimum (map (fuelTot positions) [minPosition .. maxPosition])
  where
    positions = map (read . unpack) $ splitOn (pack ",") (pack content)
    minPosition = minimum positions
    maxPosition = maximum positions

fuelTo :: Int -> Int -> Int
fuelTo start end = abs (start - end)

fuelTot :: [Int] -> Int -> Int
fuelTot starts end = sum . map (fuelTo end) $ starts

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
