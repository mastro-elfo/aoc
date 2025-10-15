import Data.List (find, groupBy)
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)
import Data.Tree (Tree (Node, rootLabel, subForest), drawTree, unfoldTree)

solution :: String -> Int
solution content = count (tree root (filter (/= root) (dedup (lefts ++ rights))) edges)
  where
    edges = map parse . lines $ content
    lefts = map fst edges
    rights = map snd edges
    root = fromJust . find (`notElem` rights) $ lefts

count :: Tree String -> Int
count = helper 0
  where
    helper c (Node a xs) = c + (sum . map (helper (c + 1)) $ xs)

tree :: String -> [String] -> [(String, String)] -> Tree String
tree root nodes edges = Node {subForest = [tree b (filter (/= root) nodes) edges | (a, b) <- edges, a == root], rootLabel = root}

dedup :: (Ord a) => [a] -> [a]
dedup = toList . fromList

parse :: String -> (String, String)
parse xs = (head parts, last parts)
  where
    parts = splitOn ')' xs

splitOn :: Char -> String -> [String]
splitOn = helper []
  where
    helper o _ [] = o
    helper [] c (x : xs)
      | x == c = helper [[]] c xs
      | otherwise = helper [[x]] c xs
    helper o c (x : xs)
      | x == c = helper (o ++ [[]]) c xs
      | otherwise = helper (init o ++ [last o ++ [x]]) c xs

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
