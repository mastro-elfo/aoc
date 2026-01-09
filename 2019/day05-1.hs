{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

data Mode = P | I

type Program = [Int]

solution :: String -> Int
solution = execute [1] 0 0 . parse

execute :: [Int] -> Int -> Int -> Program -> Int
execute ins ous cur prg
  | op == "99" = ous
  | op == "01" = execute ins ous (cur + 4) (substitute (position 3 cur prg) (value mod1 1 cur prg + value mod2 2 cur prg) prg)
  | op == "02" = execute ins ous (cur + 4) (substitute (position 3 cur prg) (value mod1 1 cur prg * value mod2 2 cur prg) prg)
  | op == "03" = execute (tail ins) ous (cur + 2) (substitute (position 1 cur prg) (head ins) prg)
  | op == "04" = execute ins (value mod1 1 cur prg) (cur + 2) prg
  | otherwise = 0
  where
    (mod3, mod2, mod1, op) = parseOpCode . left50pad . show . (!! cur) $ prg

substitute :: Int -> Int -> Program -> Program
substitute pos val prg = take pos prg ++ [val] ++ drop (pos + 1) prg

value :: Mode -> Int -> Int -> Program -> Int
value I n cur prg = (!! (n + cur)) prg
value P n cur prg = (!! (!! (n + cur)) prg) prg

position :: Int -> Int -> Program -> Int
position n cur = (!! (n + cur))

parseOpCode :: String -> (Mode, Mode, Mode, String)
parseOpCode xs =
  ( char2mode . (!! 0) $ xs,
    char2mode . (!! 1) $ xs,
    char2mode . (!! 2) $ xs,
    take 2 . drop 3 $ xs
  )

char2mode :: Char -> Mode
char2mode '0' = P
char2mode '1' = I

left50pad :: String -> String
left50pad xs
  | length xs > 4 = xs
  | otherwise = replicate (5 - length xs) '0' ++ xs

parse :: String -> Program
parse "" = []
parse xs = (read . takeWhile (/= ',') $ xs) : parse (tail . dropWhile (/= ',') $ xs)

main :: IO ()
main = do readFile "day05.dat" >>= print . solution
