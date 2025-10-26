import Data.List (findIndex, zip4)
import Data.Maybe (fromJust)

solution :: String -> Int
solution content = 4 + (fromJust . findIndex (not . repeating) $ zip4 content (drop 1 content) (drop 2 content) (drop 3 content))

repeating :: (Eq a) => (a, a, a, a) -> Bool
repeating (x, y, z, t) = x == y || x == z || x == t || y == z || y == t || z == t

main :: IO ()
main = do readFile "day06.dat" >>= print . solution
