import Data.List
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)
import Data.Tree (Tree (Node, rootLabel, subForest))
import Debug.Trace (trace)

solution :: String -> Int
solution content = (length . dropWhile (/= youRoot) $ youBranch) + (length . dropWhile (/= sanRoot) $ sanBranch) - 2
  where
    edges = map parse . lines $ content
    lefts = map fst edges
    rights = map snd edges
    root = fromJust . find (`notElem` rights) $ lefts
    aTree = tree root (filter (/= root) (dedup (lefts ++ rights))) edges
    youBranch = branch "YOU" aTree
    sanBranch = branch "SAN" aTree
    (youRoot, sanRoot) = head . dropWhile (uncurry (==)) $ zip youBranch sanBranch

branch :: (Eq a) => a -> Tree a -> [a]
branch a (Node r xs)
  | a == r = [r]
  | isRightBranch = r : fromJust (find (not . null) subtrees)
  | otherwise = []
  where
    subtrees = map (branch a) xs
    isRightBranch = not (all null subtrees)

treeElem :: (Eq a) => a -> Tree a -> Bool
treeElem a (Node r xs)
  | a == r = True
  | otherwise = any (treeElem a) xs

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
