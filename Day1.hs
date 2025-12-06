import Data.Char

type Rotation = Int

main :: IO ()
main = do
    rotations <- fmap readRotation <$> lines <$> readFile "Day1.txt"

    -- Part 1
    print
        $ length $ filter (== 0)
        $ scanl applyRotation 50 rotations

    -- Part 2
    print
        $ length $ filter (== 0)
        $ scanl applyRotation 50
        $ concat $ fmap (\i -> take (abs i) $ repeat (signum i)) rotations

-- | Apply the given `Rotation`, this function will wrap positions such that returned values are
-- always in the range `[0, 100)`.
applyRotation :: Int -> Rotation -> Int
applyRotation pos rotation = (pos + rotation + 100) `mod` 100

-- | Read a rotation, valid forms are 'Li' or `Ri`, where `i` is an integer.
readRotation :: String -> Rotation
readRotation ('L':i) = negate (read i)
readRotation ('R':i) = read i
