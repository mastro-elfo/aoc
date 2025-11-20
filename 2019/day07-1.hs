import Data.List (permutations)

type Program = [Int]

type State = ([Int], Int, Program, Bool, Int)

solution :: String -> Int
solution content = maximum (map (amplify program) phases)
  where
    program :: Program
    program = map read . split ',' $ content
    phases :: [[Int]]
    phases = permutations [0, 1, 2, 3, 4]

amplify :: Program -> [Int] -> Int
amplify program [pa, pb, pc, pd, pe] = out
  where
    inb = run program [pa, 0]
    inc = run program [pb, inb]
    ind = run program [pc, inc]
    ine = run program [pd, ind]
    out = run program [pe, ine]

run :: Program -> [Int] -> Int
run program inputs = helper (inputs, 0, program, False, 0)
  where
    helper :: State -> Int
    helper (_, _, _, True, o) = o
    helper state = helper (execute state)

execute :: State -> State
execute state
  | opcode == "01" = (inputs, current + 4, replaceValue program (getValue state 1 fstMod + getValue state 2 sndMod) ((!! (current + 3)) program), False, output)
  | opcode == "02" = (inputs, current + 4, replaceValue program (getValue state 1 fstMod * getValue state 2 sndMod) ((!! (current + 3)) program), False, output)
  | opcode == "03" = (tail inputs, current + 2, replaceValue program ((!! 0) inputs) ((!! (current + 1)) program), False, output)
  | opcode == "04" = (inputs, current + 2, replaceValue program (getValue state 1 fstMod) ((!! (current + 1)) program), False, getValue state 1 fstMod)
  | opcode == "05" = (inputs, if' (getValue state 2 sndMod) (current + 3) (getValue state 1 fstMod /= 0), program, False, output)
  | opcode == "06" = (inputs, if' (getValue state 2 sndMod) (current + 3) (getValue state 1 fstMod == 0), program, False, output)
  | opcode == "07" = (inputs, current + 4, replaceValue program (if' 1 0 (getValue state 1 fstMod < getValue state 2 sndMod)) ((!! (current + 3)) program), False, output)
  | opcode == "08" = (inputs, current + 4, replaceValue program (if' 1 0 (getValue state 1 fstMod == getValue state 2 sndMod)) ((!! (current + 3)) program), False, output)
  | opcode == "99" = (inputs, current, program, True, output)
  where
    (inputs, current, program, _, output) = state
    digits = leftPad '0' 5 (show ((!! current) program))
    fstMod = (!! 2) digits
    sndMod = (!! 1) digits
    opcode = (!! 3) digits : (!! 4) digits : ""

getValue :: State -> Int -> Char -> Int
getValue (_, current, program, _, _) param '1' = (!! (current + param)) program
getValue (_, current, program, _, _) param _ = getAt program (current + param)

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
