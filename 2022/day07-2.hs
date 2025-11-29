import Data.List (isSuffixOf, sort)
import Data.Text (intercalate, pack, split, splitOn, unpack)
import Debug.Trace (trace)

data Type = D | F deriving (Show, Eq)

type File = ([String], Int, Type)

type State = ([String], [File])

total :: Int
total = 70_000_000

required :: Int
required = 30_000_000

solution :: String -> Int
solution content = (!! 0) . sort . filter (>= toFree) . map (dirSize files) . filter isDirectory $ files
  where
    (_, files) = foldl parse ([], []) . lines $ content
    occupied = dirSize files (["/"], 0, D)
    toFree = required - total + occupied

dirSize :: [File] -> File -> Int
dirSize files (_, size, F) = size
dirSize files dir = sum . map fileSize . filter (isContainedIn dir) $ files

fileName :: File -> [String]
fileName = fstOf3

fileSize :: File -> Int
fileSize = sndOf3

isDirectory :: File -> Bool
isDirectory = (== D) . trdOf3

isContainedIn :: File -> File -> Bool
isContainedIn f1 f2 = fileName f1 `isSuffixOf` fileName f2

size :: [File] -> File -> Int
size files file = sum . map fileSize . filter (file `isContainedIn`) $ files

parse :: State -> String -> State
parse state ('$' : ' ' : 'l' : 's' : _) = state
parse (current, files) ('$' : ' ' : 'c' : 'd' : ' ' : '.' : '.' : _) = (tail current, files)
parse (current, files) ('$' : ' ' : 'c' : 'd' : ' ' : xs) = (xs : current, files)
parse (current, files) ('d' : 'i' : 'r' : ' ' : xs) = (current, (xs : current, 0, D) : files)
parse (current, files) line = (current, (name : current, read size, F) : files)
  where
    [size, name] = words line

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a

sndOf3 :: (a, b, c) -> b
sndOf3 (_, b, _) = b

trdOf3 :: (a, b, c) -> c
trdOf3 (_, _, c) = c

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
