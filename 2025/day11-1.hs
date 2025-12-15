import Data.List (find)
import Data.Maybe (fromJust)

type Device = (String, [String])

solution :: String -> Int
solution content = count (map parse . lines $ content) "you"

count :: [Device] -> String -> Int
count _ "out" = 1
count ds name = sum . map (count ds) . snd . fromJust . find ((== name) . fst) $ ds

parse :: String -> Device
parse line =
  ( init . head . words $ line,
    tail . words $ line
  )

main :: IO ()
main = do readFile "day11.dat" >>= print . solution
