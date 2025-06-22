import System.IO
import Data.Set (fromList)

type Position = (Int, Int)

solution :: String -> Int
solution = length . fromList . helper [(0,0)]
    where
        helper v [] = v
        helper v (x:xs) = helper ((move (head v) x):v) xs

move :: Position -> Char -> Position
move (x,y) '^' = (x, y+1)
move (x,y) 'v' = (x, y-1)
move (x,y) '<' = (x-1, y)
move (x,y) '>' = (x+1, y)

main :: IO ()
main = do
    handle <- openFile "day03.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content