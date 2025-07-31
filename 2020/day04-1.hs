solution :: String -> Int
solution = 
    length 
    . filter (\x -> all (flip includes x) ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"]) 
    . foldl (\ acc cur -> if' (cur == "") ("":acc) ((head acc ++ " " ++ cur):tail acc)) [""] 
    . lines

includes :: String -> String -> Bool
includes "" _ = True
includes _ "" = False
includes (x:xs) (y:ys)
    | x == y = helper xs ys || includes (x:xs) ys
    | otherwise = includes (x:xs) ys
    where
        helper "" _ = True
        helper _ "" = False
        helper (x:xs) (y:ys)
            | x == y = helper xs ys
            | otherwise = False


if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
