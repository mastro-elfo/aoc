import System.IO

type Position = (Int, Int, Int)
type Instruction = (String, Int)

solution :: String -> Int
solution content = multiply . foldl move (0,0,0) . map parse . lines $ content
    where
        multiply (x, y, _) = x * y

move :: Position -> Instruction -> Position
move (forward, depth, aim) ("forward", amount) = (forward + amount, depth + aim * amount, aim)
move (forward, depth, aim) ("up", amount) = (forward , depth, aim - amount)
move (forward, depth, aim) ("down", amount) = (forward, depth, aim + amount)

parse :: String -> Instruction
parse xs = (head . words $ xs, read . last . words $ xs)

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content