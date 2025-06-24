import System.IO

solution :: (Integral a) => a -> a
solution content = half + diff
  where
    root = ceiling . sqrt . fromIntegral $ content
    side = ternary (mod root 2 == 1) root (root + 1)
    square = (side - 2) * (side - 2)
    half = div side 2
    diff = minimum . map (abs . (\x -> x - content)) $ [square + half, square + half + side - 1, square + half + (side - 1) * 2, square + half + (side - 1) * 3]

ternary :: Bool -> p -> p -> p
ternary cond truty falsy
  | cond = truty
  | otherwise = falsy

main :: IO ()
main = do print . solution $ 325489
