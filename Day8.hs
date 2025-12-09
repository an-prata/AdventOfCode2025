import Data.Char
import Data.List
import Data.Function
import Control.Monad.State.Lazy

import qualified Data.Set as S

data Junction = Junction Int Int Int
    deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- fmap (fmap (filter isDigit) . splitAtAll ',') <$> lines <$> readFile "Day8.txt"
    let coords = fmap ((\[x, y, z] -> Junction x y z) . fmap read) input :: [Junction]
    let pairs = sortBy (compare `on` (uncurry distance)) $ S.toList $ S.fromList $ fmap (\(a, b) -> (a `min` b, a `max` b)) $ filter (\(a, b) -> a /= b) $ pairsOf coords

    -- Part 1
    let n = 1000
    print $ product $ take 3 $ reverse $ sort $ fmap S.size $ connectN n pairs

connectN :: Int -> [(Junction, Junction)] -> [S.Set Junction]
connectN n junctions = connectMax $ connectN' n [] pairSets
  where
    pairSets = fmap (\(a, b) -> S.fromList [a, b]) junctions

    connectN' _ acc [] = acc
    connectN' 0 acc (pair:pairs) = acc
    connectN' n acc (pair:pairs) = connectN' (n - 1) (connect acc pair) pairs

    connectMax l =
        let l' = foldl (\acc i -> connect acc i) [] l
        in if l == l'
            then l'
            else connectMax l'

connect :: [S.Set Junction] -> S.Set Junction -> [S.Set Junction]
connect (circut:cs) conn = if any ((`any` circut) . (==)) conn
    then (S.union circut conn) : cs
    else circut : connect cs conn
connect [] conn = [conn]

pairsOf :: [a] -> [(a, a)]
pairsOf as = concat $ fmap (\a -> fmap (a,) as) as

distance :: Junction -> Junction -> Double
distance (Junction x y z) (Junction x' y' z')
    = sqrt $ fromIntegral $ (x' - x) ^ 2 + (y' - y) ^ 2 + (z' - z) ^ 2

-- | Split a `String` at all occurances of the given `Char`.
splitAtAll :: Char -> String -> [String]
splitAtAll _ [] = []
splitAtAll split s = takeWhile (/= split) s : splitAtAll split (drop 1 $ dropWhile (/= split) s)
