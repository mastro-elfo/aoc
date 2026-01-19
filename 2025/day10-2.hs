import Data.Bifunctor qualified
import Data.List (subsequences)
import Data.Text (pack, splitOn, unpack)

type Button = [Int]

type Joltage = [Int]

type Machine = (Joltage, [Button])

solution :: String -> Int
solution = sum . map (minimum . solve . parse) . lines

solve :: Machine -> [Int]
solve (joltage, buttons)
  | any (< 0) joltage = []
  | all (== 0) joltage = [0]
  | otherwise =
      concatMap
        ( ( \(size, result) ->
              map (\sol -> size + 2 * sol) $ solve (result, buttons)
          )
            . Data.Bifunctor.second (map (`div` 2))
        )
        . filter (all even . snd)
        . map (\sub -> (length sub, foldl (zipWith (-)) joltage sub))
        . subsequences
        $ buttons

parse :: String -> Machine
parse line =
  ( joltage,
    map (parseButton (length joltage)) . init . drop 1 . words $ line
  )
  where
    joltage = parseJolt . last . words $ line

parseJolt :: String -> Joltage
parseJolt = map (read . unpack) . splitOn (pack ",") . pack . drop 1 . init

parseButton :: Int -> String -> Button
parseButton size line = map (\i -> t 1 0 (i `elem` (map (read . unpack) . splitOn (pack ",") . pack . drop 1 . init $ line))) [0 .. size]

t :: a -> a -> Bool -> a
t a _ True = a
t _ a False = a

main :: IO ()
main = do readFile "day10.dat" >>= print . solution
