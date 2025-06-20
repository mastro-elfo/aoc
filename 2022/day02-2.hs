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
parse xs = (opponent, playerToHand opponent . head . last . words $ xs)
    where
        opponent = opponentToHand . head . head . words $ xs

opponentToHand :: Char -> Char
opponentToHand 'A' = 'R'
opponentToHand 'B' = 'P'
opponentToHand 'C' = 'S'

playerToHand :: Char -> Char -> Char
playerToHand 'R' 'X' = 'S'
playerToHand 'R' 'Y' = 'R'
playerToHand 'R' 'Z' = 'P'
playerToHand 'P' 'X' = 'R'
playerToHand 'P' 'Y' = 'P'
playerToHand 'P' 'Z' = 'S'
playerToHand 'S' 'X' = 'P'
playerToHand 'S' 'Y' = 'S'
playerToHand 'S' 'Z' = 'R'

main :: IO ()
main = do
    handle <- openFile "day02.dat" ReadMode
    content <- hGetContents handle
    print . solution $ content