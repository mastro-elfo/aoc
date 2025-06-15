import System.IO

type Position = (Int, Int)

solution :: String -> String
solution content = map toCode . map (followInstructions (0, 2)) . concatInstructions [] . lines $ content

concatInstructions :: [String] -> [String] -> [String]
concatInstructions xs [] = xs
concatInstructions [] (y:ys) = concatInstructions [y] ys
concatInstructions xs (y:ys) = concatInstructions (xs ++ [last xs ++ y]) ys

toCode :: Position -> Char
toCode (2,0) = '1'
toCode (1,1) = '2'
toCode (2,1) = '3'
toCode (3,1) = '4'
toCode (0,2) = '5'
toCode (1,2) = '6'
toCode (2,2) = '7'
toCode (3,2) = '8'
toCode (4,2) = '9'
toCode (1,3) = 'A'
toCode (2,3) = 'B'
toCode (3,3) = 'C'
toCode (2,4) = 'C'
toCode _ = ' '

followInstructions :: Position -> String -> Position
followInstructions p [] = p
followInstructions p (x:xs) = followInstructions (follow x p) xs

follow :: Char -> Position -> Position
follow 'U' (0,2) = (0,2)
follow 'U' (1,1) = (1,1)
follow 'U' (2,0) = (2,0)
follow 'U' (3,1) = (3,1)
follow 'U' (4,2) = (4,2)
follow 'U' (x,y) = (x, y-1)
follow 'D' (0,2) = (0,2)
follow 'D' (1,3) = (1,3)
follow 'D' (2,4) = (2,4)
follow 'D' (3,3) = (3,3)
follow 'D' (4,2) = (4,2)
follow 'D' (x,y) = (x, y+1)
follow 'L' (2,0) = (2,0)
follow 'L' (1,1) = (1,1)
follow 'L' (0,2) = (0,2)
follow 'L' (1,3) = (1,3)
follow 'L' (2,4) = (2,4)
follow 'L' (x,y) = (x-1, y)
follow 'R' (2,0) = (2,0)
follow 'R' (3,1) = (3,1)
follow 'R' (4,2) = (4,2)
follow 'R' (3,3) = (3,3)
follow 'R' (2,4) = (2,4)
follow 'R' (x,y) = (x+1, y)

follow _ current = current

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content