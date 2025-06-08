import System.IO
import Data.Char

solution :: String -> Int
solution content = foldr (+) 0 . map (\(x,_) -> x) . filter (\(x,y) -> x == y) $ zip parsed ((tail parsed) ++ [head parsed])
    where
        parsed = map digitToInt . trim $ content


trim :: String -> String
trim = filter (\x -> x /= '\n')

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content