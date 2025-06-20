import System.IO

type Position = (Int, Int)
type Instruction = (String, Int)

solution :: String -> Int
solution content = multiply . foldl move (0,0) . map parse . lines $ content
    where
        multiply (x, y) = x * y

move :: Position -> Instruction -> Position
move (forward, depth) ("forward", amount) = (forward + amount, depth)
move (forward, depth) ("up", amount) = (forward , depth - amount)
move (forward, depth) ("down", amount) = (forward, depth + amount)

parse :: String -> Instruction
parse xs = (head . words $ xs, read . last . words $ xs)

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content