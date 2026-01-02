import Distribution.Simple.Utils (safeTail)

solution :: String -> Int
solution = (!! 0) . execute 0 . adjustInput 12 2 . parse

execute :: Int -> [Int] -> [Int]
execute cur prg
  | (!! cur) prg == 99 = prg
  | (!! cur) prg == 1 = execute (cur + 4) (substitute (lkup (cur + 3) prg) (eval (+) prg cur) prg)
  | (!! cur) prg == 2 = execute (cur + 4) (substitute (lkup (cur + 3) prg) (eval (*) prg cur) prg)

eval :: (Int -> Int -> Int) -> [Int] -> Int -> Int
eval op lst pos = op (lkupr (pos + 1) lst) (lkupr (pos + 2) lst)

lkup :: Int -> [Int] -> Int
lkup pos = (!! pos)

lkupr :: Int -> [Int] -> Int
lkupr pos lst = (!! (!! pos) lst) lst

substitute :: Int -> Int -> [Int] -> [Int]
substitute pos val lst = take pos lst ++ [val] ++ drop (pos + 1) lst

adjustInput :: Int -> Int -> [Int] -> [Int]
adjustInput noun verb (a : _ : _ : xs) = a : noun : verb : xs

parse :: String -> [Int]
parse "" = []
parse xs = (read . takeWhile (/= ',') $ xs) : parse (safeTail . dropWhile (/= ',') $ xs)

main :: IO ()
main = do readFile "day02.dat" >>= print . solution
