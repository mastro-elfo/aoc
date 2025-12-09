import Data.List (group, sort, sortBy)
import Data.Ord qualified as Ordering
import Debug.Trace (trace)

data Poker = HighCard | OnePair | TwoPair | ThreeOfKind | FullHouse | FourOfKind | FiveOfKind deriving (Eq, Ord, Show)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show)

type Hand = ([Card], Poker, Int)

solution :: String -> Int
solution content = sum (zipWith (curry (\(index, (_, _, bid)) -> index * bid)) [1 ..] (sortBy ordering . map parse . lines $ content))

ordering :: Hand -> Hand -> Ordering
ordering (ca, ta, _) (cb, tb, _)
  | ta > tb = Ordering.GT
  | ta < tb = Ordering.LT
  | (!! 0) ca > (!! 0) cb = Ordering.GT
  | (!! 0) ca < (!! 0) cb = Ordering.LT
  | (!! 1) ca > (!! 1) cb = Ordering.GT
  | (!! 1) ca < (!! 1) cb = Ordering.LT
  | (!! 2) ca > (!! 2) cb = Ordering.GT
  | (!! 2) ca < (!! 2) cb = Ordering.LT
  | (!! 3) ca > (!! 3) cb = Ordering.GT
  | (!! 3) ca < (!! 3) cb = Ordering.LT
  | (!! 4) ca > (!! 4) cb = Ordering.GT
  | (!! 4) ca < (!! 4) cb = Ordering.LT
  | (!! 5) ca > (!! 5) cb = Ordering.GT
  | (!! 5) ca < (!! 5) cb = Ordering.LT
  | otherwise = Ordering.EQ

parseType :: String -> Poker
parseType cards
  | isFiveOfKind cards = FiveOfKind
  | isFourOfKind cards = FourOfKind
  | isFullHouse cards = FullHouse
  | isThreeOfKind cards = ThreeOfKind
  | isTwoPair cards = TwoPair
  | isOnePair cards = OnePair
  | isHighCard cards = HighCard
  | otherwise = trace (show (map length . group . sort $ cards)) HighCard

isFiveOfKind :: String -> Bool
isFiveOfKind (first : cards) = all (== first) cards

isFourOfKind :: String -> Bool
isFourOfKind = (== [1, 4]) . sort . map length . group . sort

isFullHouse :: String -> Bool
isFullHouse = (== [2, 3]) . sort . map length . group . sort

isThreeOfKind :: String -> Bool
isThreeOfKind = (== [1, 1, 3]) . sort . map length . group . sort

isTwoPair :: String -> Bool
isTwoPair = (== [1, 2, 2]) . sort . map length . group . sort

isOnePair :: String -> Bool
isOnePair = (== [1, 1, 1, 2]) . sort . map length . group . sort

isHighCard :: String -> Bool
isHighCard = (== [1, 1, 1, 1, 1]) . sort . map length . group . sort

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = Ten
parseCard 'J' = Jack
parseCard 'Q' = Queen
parseCard 'K' = King
parseCard 'A' = Ace

parse :: String -> Hand
parse line =
  ( map parseCard . head . words $ line,
    parseType . head . words $ line,
    read . last . words $ line
  )

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
