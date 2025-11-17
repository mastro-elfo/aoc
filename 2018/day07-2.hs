import Data.Bifunctor (second)
import Data.Char (ord)
import Data.List (sort)
import Data.Set (fromList, toList)

type Step = (String, [String])

type Worker = (String, Int)

solution :: String -> Int
solution content = unwind steps
  where
    lns = filter (not . null) . lines $ content
    names = toList . fromList $ (map ((!! 1) . words) lns ++ map ((!! 2) . reverse . words) lns)
    steps = map (\name -> (name, map (!! 1) . filter ((== name) . (!! 2) . reverse) . map words $ lns)) names

unwind :: [Step] -> Int
unwind = helper 0 (replicate 5 ("", 0))
  where
    helper :: Int -> [Worker] -> [Step] -> Int
    helper o _ [] = o - 1
    helper o ws xs = helper (o + 1) ws3 copy1
      where
        ws1 = map (\(step, time) -> (step, max 0 (time - 1))) ws
        endedSteps = map fst . filter (\w -> (not . isIdle $ w) && hasEnded w) $ ws1
        copy1 = map (second (filter (`notElem` endedSteps))) . filter (\step -> fst step `notElem` endedSteps) $ xs
        ws2 = map (\w -> if' ("", 0) w (hasEnded w)) ws1
        workingSteps = map fst ws2
        availableSteps = sort . map fst . filter (\(name, reqs) -> (name `notElem` workingSteps) && null reqs) $ copy1
        ws3 = assignJob availableSteps ws2

assignJob :: [String] -> [Worker] -> [Worker]
assignJob [] ws = ws
assignJob _ [] = []
assignJob (x : xs) (w : ws)
  | isIdle w = (x, 60 + jobTime x) : assignJob xs ws
  | otherwise = w : assignJob (x : xs) ws

isIdle :: Worker -> Bool
isIdle ("", _) = True
isIdle _ = False

hasEnded :: Worker -> Bool
hasEnded (_, 0) = True
hasEnded _ = False

jobTime :: String -> Int
jobTime x = ord (head x) - ord 'A' + 1

if' :: a -> a -> Bool -> a
if' a _ True = a
if' _ a False = a

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
