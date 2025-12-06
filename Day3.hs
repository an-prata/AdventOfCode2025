import Data.Char
import Control.Monad
import Data.List (deleteFirstsBy)

main :: IO ()
main = do
    batteries <- lines <$> readFile "Day3.txt"

    -- Part 1
    print $ sum $ fmap (read . joltage 2) batteries

    -- Part 2
    print $ sum $ fmap (read . joltage 12) batteries

joltage :: Ord a => Int -> [a] -> [a]
joltage 0 _ = []
joltage n l@(x:xs) = if x >= maximum (take (length l - (n - 1)) l) then x : joltage (n - 1) xs else joltage n xs
