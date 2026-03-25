solution :: String -> String
solution _ = "Nothing to do"

main :: IO ()
main = do readFile "day12.dat" >>= print . solution
