import Data.List (zip4)

type IPV7 = ([String], [String])

solution :: String -> Int
solution = length . filter supportTLS . map parse . lines

supportTLS :: IPV7 -> Bool
supportTLS (net, hnet) = any hasABBA net && not (any hasABBA hnet)

hasABBA :: String -> Bool
hasABBA (a : b : c : d : xs)
  | a == d && b == c && a /= b = True
  | otherwise = hasABBA (b : c : d : xs)
hasABBA _ = False

parse :: String -> IPV7
parse = helper ([], []) False
  where
    helper o _ "" = o
    helper (nets, hnets) _ ('[' : xs) = helper (nets, "" : hnets) True xs
    helper (nets, hnets) _ (']' : xs) = helper ("" : nets, hnets) False xs
    helper (nets, hnets) True (x : xs) = helper (nets, (x : head hnets) : tail hnets) True xs
    helper ([], hnets) False (x : xs) = helper ([x : ""], hnets) False xs
    helper (nets, hnets) False (x : xs) = helper ((x : head nets) : tail nets, hnets) False xs

main :: IO ()
main = do readFile "day07.dat" >>= print . solution
