import System.IO
import Data.List

solution :: String -> Int
solution content = foldr (+) 0 . take 3 . reverse . sort . map (foldr (+) 0) . parse [[]] . lines $ content

parse :: [[Int]] -> [String] -> [[Int]]
parse elves [] = elves
parse elves (x:xs)
    | length x == 0 = parse (elves ++ [[]]) xs
    | otherwise = parse ((init elves) ++ [last elves ++ [read x]]) xs

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content