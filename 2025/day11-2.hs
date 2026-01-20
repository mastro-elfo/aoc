import Data.Bifunctor qualified
import Data.Map qualified
import Data.Maybe (fromJust)

type Devices = Data.Map.Map String [String]

type Cache = Data.Map.Map (String, Bool, Bool) Int

solution :: String -> Int
solution content = fst (count Data.Map.empty (Data.Map.fromList . map parse . lines $ content) "svr" False False)

count :: Cache -> Devices -> String -> Bool -> Bool -> (Int, Cache)
count cache _ "out" True True = (1, cache)
count cache _ "out" _ False = (0, cache)
count cache _ "out" False _ = (0, cache)
count cache ds name fft dac
  | Data.Map.member key cache = (fromJust . Data.Map.lookup key $ cache, cache)
  | otherwise = Data.Bifunctor.second (Data.Map.insert key (fst result)) result
  where
    key = (name, fft, dac)
    result =
      foldl
        (\acc target -> helper acc (count (snd acc) ds target (fft || name == "fft") (dac || name == "dac")))
        (0, cache)
        . fromJust
        . Data.Map.lookup name
        $ ds

helper :: (Int, Cache) -> (Int, Cache) -> (Int, Cache)
helper (a1, c1) (a2, c2) = (a1 + a2, Data.Map.union c1 c2)

parse :: String -> (String, [String])
parse line =
  ( init . head . words $ line,
    tail . words $ line
  )

main :: IO ()
main = do readFile "day11.dat" >>= print . solution
