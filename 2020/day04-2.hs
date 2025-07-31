solution :: String -> Int
solution = 
    length
    . filter isValid
    . foldl (\ acc cur -> if' (cur == "") ("":acc) ((head acc ++ " " ++ cur):tail acc)) [""] 
    . lines

isValid :: String -> Bool
isValid xs = all (\f -> f xs) [isValidByr, isValidIyr, isValidEyr, isValidHgt, isValidHcl, isValidEcl, isValidPid]

isValidByr :: String -> Bool
isValidByr xs = index /= -1 && 1920 <= byr && byr <= 2002
    where
        index = indexOf "byr:" xs
        byr = (read . takeWhile (/= ' ') . drop (index+4) $ xs) :: Int

isValidIyr :: String -> Bool
isValidIyr xs = index /= -1 && 2010 <= iyr && iyr <= 2020
    where
        index = indexOf "iyr:" xs
        iyr = (read . takeWhile (/= ' ') . drop (index+4) $ xs) :: Int

isValidEyr :: String -> Bool
isValidEyr xs = index /= -1 && 2020 <= eyr && eyr <= 2030
    where
        index = indexOf "eyr:" xs
        eyr = (read . takeWhile (/= ' ') . drop (index+4) $ xs) :: Int

isValidHgt :: String -> Bool
isValidHgt xs = index /= -1 && (
    (cm && 150 <= hgt && hgt <= 193)
    || (inch && 59 <= hgt && hgt <= 76))
    where
        index = indexOf "hgt:" xs
        val = takeWhile (/= ' ') . drop (index+4) $ xs
        num = takeWhile (`elem` "1234567890") val
        cm = last val == 'm'
        inch = last val == 'n'
        hgt = (read num) :: Int


isValidHcl :: String -> Bool
isValidHcl xs = index /= -1 && length hcl == 6 && all (`elem` "1234567890abcdef") hcl
    where
        index = indexOf "hcl:#" xs
        hcl = takeWhile (/= ' ') . drop (index+5) $ xs

isValidEcl :: String -> Bool
isValidEcl xs = index /= -1 && any (== ecl) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    where
        index = indexOf "ecl:" xs
        ecl = takeWhile (/= ' ') . drop (index+4) $ xs

isValidPid :: String -> Bool
isValidPid xs = index /= -1 && length pid == 9 && all (`elem` "1234567890") pid
    where
        index = indexOf "pid:" xs
        pid = takeWhile (/= ' ') . drop (index+4) $ xs

indexOf :: String -> String -> Int
indexOf _ "" = -1
indexOf xs ys = helper 0 xs ys
    where
        helper n "" "" = n
        helper n "" (y:yx) = n
        helper _ (x:xs) "" = -1
        helper n (x:xs) (y:ys)
            |  x == y = if' (helper' xs ys) n (helper (n+1) (x:xs) ys)
            |  otherwise = helper (n+1) (x:xs) ys
        helper' "" "" = True
        helper' (x:xs) "" = False
        helper' "" (y:ys) = True
        helper' (x:xs) (y:ys) = x == y && helper' xs ys

includes :: String -> String -> Bool
includes "" _ = True
includes _ "" = False
includes (x:xs) (y:ys)
    |  x == y = helper xs ys || includes (x:xs) ys
    |  otherwise = includes (x:xs) ys
    where
        helper "" _ = True
        helper _ "" = False
        helper (x:xs) (y:ys)
            |  x == y = helper xs ys
            |  otherwise = False


if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

main :: IO ()
main = do readFile "day04.dat" >>= print . solution
