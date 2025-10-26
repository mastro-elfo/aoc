import Data.List (findIndex)
import Data.Maybe (fromJust)

solution :: String -> Int
solution content =
  14
    + ( fromJust . findIndex (not . repeating) $
          zip14 content (drop 1 content) (drop 2 content) (drop 3 content) (drop 4 content) (drop 5 content) (drop 6 content) (drop 7 content) (drop 8 content) (drop 9 content) (drop 10 content) (drop 11 content) (drop 12 content) (drop 13 content)
      )

repeating :: (Eq a) => (a, a, a, a, a, a, a, a, a, a, a, a, a, a) -> Bool
repeating (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = repeatingList [a, b, c, d, e, f, g, h, i, j, k, l, m, n]

repeatingList :: (Eq a) => [a] -> Bool
repeatingList [] = False
repeatingList (x : xs) = (x `elem` xs) || repeatingList xs

zip14 :: [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [(a, a, a, a, a, a, a, a, a, a, a, a, a, a)]
zip14 [] _ _ _ _ _ _ _ _ _ _ _ _ _ = []
zip14 _ [] _ _ _ _ _ _ _ _ _ _ _ _ = []
zip14 _ _ [] _ _ _ _ _ _ _ _ _ _ _ = []
zip14 _ _ _ [] _ _ _ _ _ _ _ _ _ _ = []
zip14 _ _ _ _ [] _ _ _ _ _ _ _ _ _ = []
zip14 _ _ _ _ _ [] _ _ _ _ _ _ _ _ = []
zip14 _ _ _ _ _ _ [] _ _ _ _ _ _ _ = []
zip14 _ _ _ _ _ _ _ [] _ _ _ _ _ _ = []
zip14 _ _ _ _ _ _ _ _ [] _ _ _ _ _ = []
zip14 _ _ _ _ _ _ _ _ _ [] _ _ _ _ = []
zip14 _ _ _ _ _ _ _ _ _ _ [] _ _ _ = []
zip14 _ _ _ _ _ _ _ _ _ _ _ [] _ _ = []
zip14 _ _ _ _ _ _ _ _ _ _ _ _ [] _ = []
zip14 _ _ _ _ _ _ _ _ _ _ _ _ _ [] = []
zip14 (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) (g : gs) (h : hs) (i : is) (j : js) (k : ks) (l : ls) (m : ms) (n : ns) =
  (a, b, c, d, e, f, g, h, i, j, k, l, m, n) : zip14 as bs cs ds es fs gs hs is js ks ls ms ns

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
