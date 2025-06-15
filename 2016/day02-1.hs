import System.IO

type Position = (Int, Int)

solution :: String -> String
solution content = map toCode . map (followInstructions (1, 1)) . concatInstructions [] . lines $ content

concatInstructions :: [String] -> [String] -> [String]
concatInstructions xs [] = xs
concatInstructions [] (y:ys) = concatInstructions [y] ys
concatInstructions xs (y:ys) = concatInstructions (xs ++ [last xs ++ y]) ys

toCode :: Position -> Char
toCode (0,0) = '1'
toCode (1,0) = '2'
toCode (2,0) = '3'
toCode (0,1) = '4'
toCode (1,1) = '5'
toCode (2,1) = '6'
toCode (0,2) = '7'
toCode (1,2) = '8'
toCode (2,2) = '9'

followInstructions :: Position -> String -> Position
followInstructions p [] = p
followInstructions p (x:xs) = followInstructions (follow x p) xs

follow :: Char -> Position -> Position
follow 'U' (x,y) = (x, max 0 (y-1))
follow 'D' (x,y) = (x, min 2 (y+1))
follow 'L' (x,y) = (max 0 (x-1), y)
follow 'R' (x,y) = (min 2 (x+1), y)
follow _ current = current

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content