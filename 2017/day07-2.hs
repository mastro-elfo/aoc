import Data.List (elemIndex, find)
import Data.Maybe (fromJust)
import Data.Set (fromList)
import Debug.Trace (trace)
import Distribution.Utils.Generic (fstOf3, sndOf3, trdOf3)

type Program = (String, Int, [String])

solution :: String -> Int
solution content = sndOf3 (get programs ((!! minIndex) . trdOf3 $ unblncd)) + othKey - minKey
  where
    programs = map parse . lines $ content
    unblncd = unbalanced programs
    wghts = weights programs unblncd
    minKey = theOne wghts
    othKey = head . filter (/= minKey) $ wghts
    minIndex = fromJust . elemIndex minKey $ wghts

unbalanced :: [Program] -> Program
unbalanced programs = innermost . filter (isUnbalanced . weights programs) $ programs

innermost :: [Program] -> Program
innermost [x] = x
innermost (x : xs) = innermost (if' [] fs (length fs == length xxs))
  where
    xxs = x : xs
    names = map fstOf3 xxs
    topNames = fromList . concatMap trdOf3 $ xxs
    bottomNames = filter (`notElem` topNames) names
    fs = filter (\x -> fstOf3 x `notElem` bottomNames) xxs

theOne :: [Int] -> Int
theOne [x] = x
theOne (x : xs)
  | x `elem` xs = theOne (filter (/= x) xs)
  | otherwise = x

isUnbalanced :: [Int] -> Bool
isUnbalanced [] = False
isUnbalanced (x : xs) = any (/= x) xs

parse :: String -> Program
parse line = (head parts, read . (!! 1) $ parts, map noComma . drop 3 $ parts)
  where
    parts = words line

get :: [Program] -> String -> Program
get ps name = fromJust (find (\p -> fstOf3 p == name) ps)

weight :: [Program] -> Program -> Int
weight ps (_, w, names) = w + (sum . map (weight ps . get ps) $ names)

weights :: [Program] -> Program -> [Int]
weights ps (_, _, names) = map (weight ps . get ps) names

noComma :: String -> String
noComma = filter (/= ',')

if' :: a -> a -> Bool -> a
if' a _ True = a
if' _ a False = a

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
