import Data.Char (chr, ord)
import Data.List (minimum)
import Data.Set (fromList, toList)

solution :: String -> Int
solution content = minimum . map (\c -> length . keep_reacting . remove c $ clean) $ alphabeth
    where 
        clean = takeWhile (/= '\n') content
        alphabeth = toList . fromList . map lower $ clean

remove :: Char -> String -> String
remove c = filter (\x -> x /= c && lower x /= c)

keep_reacting :: String -> String
keep_reacting polymer
    | polymer == reacted = polymer
    | otherwise = keep_reacting reacted
    where
        reacted = react polymer

react :: String -> String
react (x:y:xs)
    | shouldReact x y = react xs
    | otherwise = x : react (y:xs)
react xs = xs

shouldReact :: Char -> Char -> Bool
shouldReact a b = lower a == lower b && a /= b

lower :: Char -> Char
lower a 
    | ord 'a' <= ord a && ord a <= ord 'z' = a
    | otherwise = chr (ord a + (ord 'a' - ord 'A'))

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
