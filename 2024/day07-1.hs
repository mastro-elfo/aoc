import Data.List (find)
import Data.Maybe (isJust)

data Operation = MUL | SUM

type Equation = (Int, [Int])

solution :: String -> Int
solution = sum . map getResult . filter test . map parse . lines

getResult :: Equation -> Int
getResult = fst

test :: Equation -> Bool
test (res, args) = isJust . find (\ops -> res == apply args ops) $ operations (length args - 1)

apply :: [Int] -> [Operation] -> Int
apply (x : xs) ops = foldl (\x (y, op) -> calc x y op) x $ zip xs ops

calc :: Int -> Int -> Operation -> Int
calc x y SUM = x + y
calc x y MUL = x * y

operations :: Int -> [[Operation]]
operations 0 = [[]]
operations n = map (SUM :) recursive ++ map (MUL :) recursive
  where
    recursive = operations (n - 1)

parse :: String -> Equation
parse line =
  ( read . init . head . words $ line,
    map read . tail . words $ line
  )

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
