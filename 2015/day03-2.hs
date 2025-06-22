import System.IO
import Data.Set (fromList)

type Position = (Int, Int)

solution :: String -> Int
solution content = length . fromList $ (helper[(0,0)] (evens content) ++ helper [(0,0)] (odds content))
    where
        helper v [] = v
        helper v (x:xs) = helper ((move (head v) x):v) xs

move :: Position -> Char -> Position
move (x,y) '^' = (x, y+1)
move (x,y) 'v' = (x, y-1)
move (x,y) '<' = (x-1, y)
move (x,y) '>' = (x+1, y)

evens :: [a] -> [a]
evens (x:xs) = x:odds xs
evens _ = []

odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []

main :: IO ()
main = do
    handle <- openFile "day03.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content