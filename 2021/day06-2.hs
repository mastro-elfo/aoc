import Data.List (elemIndex, find)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (fromList, toList)

type FishGroup = (Int, Int)

solution :: String -> Int
solution content = size (play 256 state)
  where
    current :: [Int]
    current = map read . split ',' $ content
    state = map (\x -> (x, length . filter (== x) $ current)) . unique $ current

size :: [FishGroup] -> Int
size = sum . map snd

play :: Int -> [FishGroup] -> [FishGroup]
play 0 fg = fg
play n fg = play (n - 1) (clean . nextDay . generate $ fg)

generate :: [FishGroup] -> [FishGroup]
generate fg = fg ++ [(9, snd (fromMaybe (0, 0) (find ((== 0) . fst) fg)))]

nextDay :: [FishGroup] -> [FishGroup]
nextDay [] = []
nextDay ((d, q) : xs)
  | d == 0 = (6, q) : nextDay xs
  | otherwise = (d - 1, q) : nextDay xs

clean :: [FishGroup] -> [FishGroup]
clean fg = filter ((/= 0) . snd) $ filter ((/= 6) . fst) fg ++ [(6, sum . map snd . filter ((== 6) . fst) $ fg)]

unique :: (Ord a) => [a] -> [a]
unique = toList . fromList

split :: (Eq a) => a -> [a] -> [[a]]
split c [] = []
split c xs
  | isJust next = take (fromJust next) xs : split c (drop (fromJust next + 1) xs)
  | otherwise = [xs]
  where
    next = elemIndex c xs

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
