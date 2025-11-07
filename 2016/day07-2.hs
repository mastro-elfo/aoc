import Data.List (isInfixOf)

type IPV7 = ([String], [String])

solution :: String -> Int
solution = length . filter supportSSL . map parse . lines

supportSSL :: IPV7 -> Bool
supportSSL (net, hnet) = any (any (\aba -> any (\part -> getBAB aba `isInfixOf` part) hnet)) ([getABAs part | part <- net])

getBAB :: String -> String
getBAB (a : b : _) = b : a : b : ""

getABAs :: String -> [String]
getABAs part = [a : b : c : "" | (a, b, c) <- zip3 part (tail part) ((tail . tail) part), a == c && a /= b]

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
