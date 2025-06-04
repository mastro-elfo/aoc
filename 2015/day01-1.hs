import System.IO

solution :: String -> Int
solution content = (countChar '(' content) - (countChar ')' content)

countChar :: Eq a => a -> [a] -> Int
countChar _ [] = 0
countChar ch (x:xs)
    | ch == x = 1 + countChar ch xs
    | otherwise = countChar ch xs

main :: IO ()
main = do
    handle <- openFile "day01.dat" ReadMode
    content <- hGetContents handle
    print . show . solution $ content