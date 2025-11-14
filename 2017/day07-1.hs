import Data.Set (fromList, toList)
import Debug.Trace (trace)

type Program = (String, [String])

solution :: String -> String
solution content = head . filter (`notElem` topNames) $ names
  where
    programs = map parse . lines $ content
    names = map fst programs
    topNames = fromList . concatMap (filter (/= "") . snd) $ programs

parse :: String -> Program
parse line = (head parts, map noComma . drop 3 $ parts)
  where
    parts = words line

noComma :: String -> String
noComma = filter (/= ',')

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
