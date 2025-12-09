import Data.List
import Data.Function
import Data.Maybe

main :: IO ()
main = do
    coords <- fmap readCoord <$> lines <$> readFile "Day9.txt"

    -- Part 1
    let areas = fmap (uncurry area) $ pairsOf coords
    print $ maximum areas

    -- Part 2
    let pairs = reverse $ sortBy (compare `on` (uncurry area)) $ pairsOf coords
    let rect = find (\(a, b) -> (wrapped a b coords) && (not $ intersected a b coords)) pairs
    print $ uncurry area <$> rect

wrapped :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Bool
wrapped (x, y) (x', y') coords
    = isJust (find isUpLeft coords)
    && isJust (find isUpRight coords)
    && isJust (find isDownLeft coords)
    && isJust (find isDownRight coords)
  where
    x1 = (x `min` x')
    x2 = (x `max` x')
    y1 = (y `min` y')
    y2 = (y `max` y')

    isUpRight   (x, y) = x >= x2 && y >= y2
    isDownRight (x, y) = x >= x2 && y <= y1
    isUpLeft    (x, y) = x <= x1 && y >= y2
    isDownLeft  (x, y) = x <= x1 && y <= y1

intersected :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Bool
intersected (x, y) (x', y') coords = any insideOfRect coords || any (uncurry crossesRect) coordEdges
  where
    x1 = (x `min` x')
    x2 = (x `max` x')
    y1 = (y `min` y')
    y2 = (y `max` y')

    insideOfRect (x, y) = x < x2 && x > x1 && y < y2 && y > y1

    crossesRect (x, y) (x', y') = if y == y'
        then xb >= x2 && xa <= x1 && y <= y2 && y >= y1
        else yb >= y2 && ya <= y1 && x <= x2 && x >= x1
      where
        xa = x `min` x'
        xb = x `max` x'
        ya = y `min` y'
        yb = y `max` y'

    coordEdges = edges (coords ++ [last coords])
    edges (a:b:ps) = (a, b) : edges (b:ps)
    edges _ = []

area :: (Int, Int) -> (Int, Int) -> Int
area (x, y) (x', y') = (abs (x' - x) + 1) * (abs (y' - y) + 1)

readCoord :: String -> (Int, Int)
readCoord s = (read $ takeWhile (/= ',') s, read $ drop 1 $ dropWhile (/= ',') s)

pairsOf :: [a] -> [(a, a)]
pairsOf xs = concat $ fmap (\x -> fmap (x,) xs) xs
