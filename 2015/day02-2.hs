import System.IO
import Data.List

type Dimensions = (Int, Int, Int)

solution :: String -> Int
solution content = foldr (+) 0 . map ribbon . map parse . lines $ content

ribbon :: Dimensions -> Int
ribbon (d0, d1, d2) = 2*a + 2*b + d0 * d1 * d2
    where
        sorted = sort [d0, d1, d2]
        a = head sorted
        b = head . drop 1 $ sorted

parse :: String -> Dimensions
parse xs = toDimensions . map read . take 3 . helper [""] $ xs
    where
        helper ys "" = ys
        helper ys (x:xs)
            | elem x "1234567890" = helper (init ys ++ [last ys ++ [x]]) xs
            | otherwise = helper (ys ++ [""]) xs

toDimensions :: [Int] -> Dimensions
toDimensions [] = (0, 0, 0)
toDimensions (a:[]) = (0, 0, 0)
toDimensions (a:b:[]) = (0, 0, 0)
toDimensions (a:b:c:_) = (a, b, c)

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content