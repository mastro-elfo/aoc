import System.IO
import Data.List (sort)

solution :: String -> Int
solution content =
    foldr (+) 0 . map (\(x,y) -> abs (x-y)) $
    zip (sort . map (\xs -> head xs) $ split) (sort . map (\xs -> last xs) $ split)
    where
        split = map (map read) . map words . lines $ content


main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content