import System.IO

type Colors = (Int, Int, Int)
type Game = (Int, [Colors])

solution :: String -> Int
solution content = sum . map power . map minimumColor . map parse . lines $ content

minimumColor :: Game -> Colors
minimumColor (_, colors) = (
    maximum . map (\(r,_,_) -> r) $ colors,
    maximum . map (\(_,g,_) -> g) $ colors,
    maximum . map (\(_,_,b) -> b) $ colors)

power :: Colors -> Int
power (r,g,b) = r * g * b

parse :: String -> Game
parse xs = (
    read . last . words . head . splitOn ':' $ xs,
    map parseCubes . splitOn ';' . last . splitOn ':' $ xs)

parseCubes :: String -> Colors
parseCubes xs = (
    read . head . words . head . orZero . filter (byColor 'r') . splitOn ',' $ xs,
    read . head . words . head . orZero . filter (byColor 'g') . splitOn ',' $ xs,
    read . head . words . head . orZero . filter (byColor 'b') . splitOn ',' $ xs)

byColor :: Char -> String -> Bool
byColor c xs = c == (head . last . words $ xs)

orZero :: [String] -> [String]
orZero [] = ["0"]
orZero xs = xs

splitOn :: Char -> String -> [String]
splitOn c xs = helper [""] c xs
    where
        helper o c "" = o
        helper o c (x:"")
            | x == c = o
            | otherwise = (init o) ++ [(last o) ++ [x]]
        helper o c (x:xs)
            | x == c = helper (o++[""]) c xs
            | otherwise = helper ((init o) ++ [(last o) ++ [x]]) c xs

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content