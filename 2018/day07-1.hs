import Data.Bifunctor (second)
import Data.List (sort)
import Data.Set (fromList, toList)

type Step = (String, [String])

solution :: String -> String
solution content = unwind steps
  where
    lns = filter (not . null) . lines $ content
    names = toList . fromList $ (map ((!! 1) . words) lns ++ map ((!! 2) . reverse . words) lns)
    steps = map (\name -> (name, map (!! 1) . filter ((== name) . (!! 2) . reverse) . map words $ lns)) names

unwind :: [Step] -> String
unwind = helper ""
  where
    helper o [] = o
    helper o xs = helper (o ++ f) (map (second (filter (/= f))) . filter ((/= f) . fst) $ xs)
      where
        f = (minimum . map fst . filter (null . snd)) xs

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
