import System.IO

type Hand = (Char, Char)

solution :: String -> Int
solution content = foldr (+) 0 . map play . map parse . lines $ content

play :: Hand -> Int
play (o, p) = (scoreByShape p) + (scoreByOutcome . outcome $ (o, p))

outcome :: Hand -> Char
outcome ('R', 'R') = 'D'
outcome ('R', 'P') = 'W'
outcome ('R', 'S') = 'L'
outcome ('P', 'R') = 'L'
outcome ('P', 'P') = 'D'
outcome ('P', 'S') = 'W'
outcome ('S', 'R') = 'W'
outcome ('S', 'P') = 'L'
outcome ('S', 'S') = 'D'


scoreByShape :: Char -> Int
scoreByShape 'R' = 1
scoreByShape 'P' = 2
scoreByShape 'S' = 3

scoreByOutcome :: Char -> Int
scoreByOutcome 'W' = 6
scoreByOutcome 'D' = 3
scoreByOutcome 'L' = 0

parse :: String -> Hand
parse xs = (opponentToHand . head . head . words $ xs, playerToHand . head . last . words $ xs)

opponentToHand :: Char -> Char
opponentToHand 'A' = 'R'
opponentToHand 'B' = 'P'
opponentToHand 'C' = 'S'

playerToHand :: Char -> Char
playerToHand 'X' = 'R'
playerToHand 'Y' = 'P'
playerToHand 'Z' = 'S'

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content