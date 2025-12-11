import Data.List (subsequences)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (pack, splitOn, unpack)

type Machine = ([Bool], [[Int]])

solution :: String -> Int
solution = sum . map (strategy . parse) . lines

strategy :: Machine -> Int
strategy (lights, buttons) = minimum . mapMaybe (test lights) . subsequences $ buttons

test :: [Bool] -> [[Int]] -> Maybe Int
test lights [] = Nothing
test lights (b : bs)
  | all not newLights = Just 1
  | otherwise = increment $ test newLights bs
  where
    newLights = push lights b

increment :: Maybe Int -> Maybe Int
increment (Just a) = Just (a + 1)
increment Nothing = Nothing

push :: [Bool] -> [Int] -> [Bool]
push lights buttons = zipWith (\k -> toggle (k `elem` buttons)) [0 ..] lights

toggle :: Bool -> Bool -> Bool
toggle True = not
toggle False = id

parse :: String -> Machine
parse line =
  ( parseLight . middle . head . words $ line,
    map (map (read . unpack) . splitOn (pack ",") . pack . middle) . middle . words $ line
  )

parseLight :: String -> [Bool]
parseLight "" = []
parseLight ('#' : xs) = True : parseLight xs
parseLight ('.' : xs) = False : parseLight xs

middle :: [a] -> [a]
middle = init . tail

if' :: a -> a -> Bool -> a
if' a _ True = a
if' _ a False = a

main :: IO ()
main = do readFile "day10.dat" >>= print . solution
