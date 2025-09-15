import Debug.Trace (trace)

type Instruction = (Int, Int, Int)
type Stacks = [[Char]]

solution :: String -> String
solution content = tops $ foldl act stacks instructions
    where
        (stacks, instructions) = parse content

tops :: Stacks -> String
tops [] = ""
tops (x:xs) = head x : tops xs

act :: Stacks -> Instruction -> Stacks
act stacks (qty, from, to) = add toMove to (remove qty from stacks)
    where toMove = reverse . take qty $ stacks !! from

remove :: Int -> Int -> Stacks -> Stacks
remove qty from stacks = take from stacks ++ [drop qty (stacks !! from)] ++ drop (from +1) stacks

add :: [Char] -> Int -> Stacks -> Stacks
add stack to stacks = take to stacks ++ [stack ++ (stacks !! to)] ++ drop (to  +1) stacks

parse :: String -> (Stacks, [Instruction])
parse content = (parseStacks part1, parseInstructions part2)
    where
        part1 = map fst . takeWhile (\(a, b) -> a /= '\n' || b /= '\n') $ zip content (tail content)
        part2 = map fst . tail . tail . dropWhile (\(a, b) -> a /= '\n' || b /= '\n') $ zip content (tail content)

parseStacks :: String -> Stacks
parseStacks content = map (\i -> filter (/= ' ') . map (!! i) $ byLine) [0 .. size-1]
    where
        byLine = map parseStackLine . filter (elem '[') . lines $ content
        size = length (byLine !! 0)

parseStackLine :: String -> [Char]
parseStackLine line = [line !! i | i <- indices]
    where
        size = (length line - 1) `quot` 4
        indices = [i * 4 + 1 | i <- [0..size]]

parseInstructions :: String -> [Instruction]
parseInstructions = map parseProcedure . lines

parseProcedure :: String -> Instruction
parseProcedure xs = (
    read . takeWhile (/= ' ') . tail . dropWhile (/= ' ') $ xs,
    (read . takeWhile (/= ' ') . tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') $ xs) -1,
    (read . takeWhile (/= ' ') . tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ') $ xs) -1
    )

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
