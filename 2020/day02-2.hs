import System.IO

type Entity = (Int, Int, Char, String)

solution :: String -> Int
solution content = length . filter isValid . map parse . lines $ content

isValid :: Entity -> Bool
isValid (fst, snd, char, password) = password !! (fst-1) == char && password !! (snd-1) /= char || password !! (snd-1) == char && password !! (fst-1) /= char

parse :: String -> Entity
parse line = (
    read . head . splitOn '-' $ line,
    read . head . splitOn ' ' . head . drop 1 . splitOn '-' $ line,
    head . head . splitOn ':' . head . drop 1 . splitOn ' ' $ line,
    last . splitOn ' ' $ line)

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