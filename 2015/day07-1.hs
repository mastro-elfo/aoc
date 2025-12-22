import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Map (Map, empty, insert, lookup, member)
import Data.Maybe (fromJust, fromMaybe)
import Prelude hiding (lookup)

data Operator = SIG | NOT | AND | OR | LSHIFT | RSHIFT deriving (Show)

type Instruction = (String, String, Operator, String)

type Cache = Map String Int

solution :: String -> Int
solution = evaluate "a" empty . map parse . lines

evaluate :: String -> Cache -> [Instruction] -> Int
evaluate key cache ins
  | member key cache = fromJust . lookup key $ cache
  | otherwise = evaluate key newCache toDo
  where
    toEval = filter (isEvaluatable cache) ins
    toDo = filter (not . isEvaluatable cache) ins
    newCache = foldl doEval cache toEval

doEval :: Cache -> Instruction -> Cache
doEval cache (left, _, SIG, t) = insert t (fromMaybe (read left) (lookup left cache)) cache
doEval cache (left, _, NOT, t) = insert t (complement $ fromMaybe (read left) (lookup left cache)) cache
doEval cache (left, right, AND, t) = insert t ((.&.) (fromMaybe (read left) (lookup left cache)) (fromMaybe (read right) (lookup right cache))) cache
doEval cache (left, right, OR, t) = insert t ((.|.) (fromMaybe (read left) (lookup left cache)) (fromMaybe (read right) (lookup right cache))) cache
doEval cache (left, right, LSHIFT, t) = insert t (shiftL (fromMaybe (read left) (lookup left cache)) (fromMaybe (read right) (lookup right cache))) cache
doEval cache (left, right, RSHIFT, t) = insert t (shiftR (fromMaybe (read left) (lookup left cache)) (fromMaybe (read right) (lookup right cache))) cache

isEvaluatable :: Cache -> Instruction -> Bool
isEvaluatable cache (left, _, SIG, _) = isNumber left || member left cache
isEvaluatable cache (left, _, NOT, _) = isNumber left || member left cache
isEvaluatable cache (left, right, AND, _) = (isNumber left || member left cache) && (isNumber right || member right cache)
isEvaluatable cache (left, right, OR, _) = (isNumber left || member left cache) && (isNumber right || member right cache)
isEvaluatable cache (left, right, LSHIFT, _) = (isNumber left || member left cache) && (isNumber right || member right cache)
isEvaluatable cache (left, right, RSHIFT, _) = (isNumber left || member left cache) && (isNumber right || member right cache)

isNumber :: String -> Bool
isNumber = all (`elem` "1234567890")

parse :: String -> Instruction
parse line
  | (!! 0) ws == "NOT" = ((!! 1) ws, "", NOT, (!! 3) ws)
  | (!! 1) ws == "AND" = ((!! 0) ws, (!! 2) ws, AND, (!! 4) ws)
  | (!! 1) ws == "OR" = ((!! 0) ws, (!! 2) ws, OR, (!! 4) ws)
  | (!! 1) ws == "LSHIFT" = ((!! 0) ws, (!! 2) ws, LSHIFT, (!! 4) ws)
  | (!! 1) ws == "RSHIFT" = ((!! 0) ws, (!! 2) ws, RSHIFT, (!! 4) ws)
  | otherwise = ((!! 0) ws, "", SIG, (!! 2) ws)
  where
    ws = words line

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
