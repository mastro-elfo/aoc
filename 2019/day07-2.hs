import Data.List (permutations)

type Program = [Int]

type State = ([Int], Int, Program, Bool, Bool, Int)

solution :: String -> Int
solution content = maximum (map (amplify program) phases)
  where
    program :: Program
    program = map read . split ',' $ content
    phases :: [[Int]]
    phases = permutations [5, 6, 7, 8, 9]

amplify :: Program -> [Int] -> Int
amplify program [pa, pb, pc, pd, pe] = helper [stateA, stateB, stateC, stateD, stateE] 0
  where
    stateA = run ([pa, 0], 0, program, False, False, 0)
    stateB = run ([pb, getOutput stateA], 0, program, False, False, 0)
    stateC = run ([pc, getOutput stateB], 0, program, False, False, 0)
    stateD = run ([pd, getOutput stateC], 0, program, False, False, 0)
    stateE = run ([pe, getOutput stateD], 0, program, False, False, 0)
    helper :: [State] -> Int -> Int
    helper (first : states) n
      | not (all getHalt states) = helper (states ++ [run (reset first [getOutput (last states)])]) (n + 1)
      | otherwise = getOutput . (!! (4 - (n `mod` 5))) $ (first : states)

getHalt :: State -> Bool
getHalt (_, _, _, _, h, _) = h

getOutput :: State -> Int
getOutput (_, _, _, _, _, o) = o

reset :: State -> [Int] -> State
reset (_, current, program, _, halt, output) inputs = (inputs, current, program, False, halt, output)

run :: State -> State
run state
  | halt = state
  | suspend = state
  | otherwise = run (execute state)
  where
    (_, _, _, suspend, halt, _) = state

execute :: State -> State
execute state
  | opcode == "01" = (inputs, current + 4, replaceValue program (getValue state 1 fstMod + getValue state 2 sndMod) ((!! (current + 3)) program), False, False, output)
  | opcode == "02" = (inputs, current + 4, replaceValue program (getValue state 1 fstMod * getValue state 2 sndMod) ((!! (current + 3)) program), False, False, output)
  | opcode == "03" = (if' inputs (tail inputs) (length inputs == 1), current + 2, replaceValue program ((!! 0) inputs) ((!! (current + 1)) program), False, False, output)
  | opcode == "04" = (inputs, current + 2, replaceValue program (getValue state 1 fstMod) ((!! (current + 1)) program), True, False, getValue state 1 fstMod)
  | opcode == "05" = (inputs, if' (getValue state 2 sndMod) (current + 3) (getValue state 1 fstMod /= 0), program, False, False, output)
  | opcode == "06" = (inputs, if' (getValue state 2 sndMod) (current + 3) (getValue state 1 fstMod == 0), program, False, False, output)
  | opcode == "07" = (inputs, current + 4, replaceValue program (if' 1 0 (getValue state 1 fstMod < getValue state 2 sndMod)) ((!! (current + 3)) program), False, False, output)
  | opcode == "08" = (inputs, current + 4, replaceValue program (if' 1 0 (getValue state 1 fstMod == getValue state 2 sndMod)) ((!! (current + 3)) program), False, False, output)
  | opcode == "99" = (inputs, current, program, True, True, output)
  where
    (inputs, current, program, _, _, output) = state
    digits = leftPad '0' 5 (show ((!! current) program))
    fstMod = (!! 2) digits
    sndMod = (!! 1) digits
    opcode = (!! 3) digits : (!! 4) digits : ""

getValue :: State -> Int -> Char -> Int
getValue (_, current, program, _, _, _) param '1' = (!! (current + param)) program
getValue (_, current, program, _, _, _) param _ = getAt program (current + param)

getAt :: Program -> Int -> Int
getAt program address = (!! (!! address) program) program

replaceValue :: Program -> Int -> Int -> Program
replaceValue prog value 0 = value : tail prog
replaceValue (p : ps) value n = p : replaceValue ps value (n - 1)

leftPad :: Char -> Int -> String -> String
leftPad char n xs
  | length xs >= n = xs
  | otherwise = replicate (n - length xs) char ++ xs

if' :: a -> a -> Bool -> a
if' a _ True = a
if' _ a False = a

split :: (Eq a) => a -> [a] -> [[a]]
split = helper []
  where
    helper o _ [] = o
    helper [] a (x : xs)
      | a == x = helper [[], []] a xs
      | otherwise = helper [[x]] a xs
    helper o a (x : xs)
      | a == x = helper (o ++ [[]]) a xs
      | otherwise = helper (init o ++ [last o ++ [x]]) a xs

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
