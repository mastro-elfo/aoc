-- TODO: Test between best and worst case
-- TODO: Tile is just the sum of "#"
-- TODO: Region requires just the total area

import Data.Text (pack, splitOn, unpack)

type Tile = [[Bool]]

type Region = ([Int], [Int])

solution :: String -> Int
solution content = length . filter (test sizes) $ regions
  where
    (tiles, regions) = parse content
    sizes = map size tiles

test :: [Int] -> Region -> Bool
test sizes ([width, height], rs) = (width * height) >= sum (zipWith (*) sizes rs)

size :: Tile -> Int
size = sum . map (length . filter id)

parse :: String -> ([Tile], [Region])
parse content = (map parseTile . init $ parts, map parseRegion . lines . last $ parts)
  where
    parts = map unpack . splitOn (pack "\n\n") . pack $ content

parseTile :: String -> Tile
parseTile = map (map (== '#')) . drop 1 . lines

parseRegion :: String -> Region
parseRegion line = (size, map read . drop 1 $ parts)
  where
    parts = words line
    size = map (read . unpack) . splitOn (pack "x") . pack . init . (!! 0) $ parts

main :: IO ()
main = do readFile "day12.dat" >>= print . solution
