import System.IO

solution :: String -> Int
solution content = (length . filter (hasnx 2) . lines $ content) * (length . filter (hasnx 3) . lines $ content)

hasnx :: Int -> String -> Bool
hasnx n xs = any (== n) $ map length [[x | x<-xs, x== y] | y<-xs]

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content
