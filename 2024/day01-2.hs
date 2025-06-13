import System.IO

solution :: String -> Int
solution content =
    foldr (+) 0 $
    map (\l -> l * (length . filter (\r -> r == l) $ rigth)) left
    where
        split = map (map read) . map words . lines $ content
        left = map (\xs -> head xs) $ split
        rigth = map (\xs -> last xs) $ split

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content