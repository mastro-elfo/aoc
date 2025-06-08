import System.IO

solution :: String -> Int
solution content = foldr (+) 0 . map (\x -> x - 2) . map (\x -> div x 3) . map (read) . lines $ content

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content