import System.IO

solution :: String -> Int
solution content = foldr (+) 0 . map (\xs -> (listMax xs) - (listMin xs)) . map (map read) . map words. lines $ content

listMax :: (Ord a) => [a] -> a
listMax (x:xs) = helper x xs
    where
        helper x [] = x
        helper x (y:ys)
            | y > x = helper y ys
            | otherwise = helper x ys

listMin :: (Ord a) => [a] -> a
listMin (x:xs) = helper x xs
    where
        helper x [] = x
        helper x (y:ys)
            | y < x = helper y ys
            | otherwise = helper x ys

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content