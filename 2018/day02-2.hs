import System.IO

solution :: String -> String
solution content = map (\(x,_) -> x) . filter (\(a,b) -> a == b) $ zip fst snd
    where
        (fst, snd) = [(fst, snd) | fst <- lines content, snd <- lines content, (distance fst snd) == 1] !! 0

distance :: String -> String -> Int
distance "" "" = 0
distance (x:xs) (y:ys)
    | x /= y = 1 + distance xs ys
    | otherwise = distance xs ys

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content
