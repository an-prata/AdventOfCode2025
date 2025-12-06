import Data.List
import Data.Char

main :: IO ()
main = do

    -- Part 1
    
    problems
        <- fmap reverse
        <$> transpose
        <$> fmap (splitAtAll isSpace . dropWhile isSpace)
        <$> lines
        <$> readFile "Day6.txt"

    print $ sum $ fmap compute problems

    -- Part 2

    problems
        <- splitRows
        <$> transpose
        <$> lines
        <$> readFile "Day6.txt"

    print $ sum $ fmap computeRow problems

-- | Compute a single problem as specified in part 1 after splitting and transposing.
compute :: [String] -> Int
compute ("+" : numbers) = sum $ fmap read numbers
compute ("*" : numbers) = product $ fmap read numbers

-- | Split a `String` on any `Char`, which satisfies the given function, removing every instance of
-- a `Char` which does.
splitAtAll :: (Char -> Bool) -> String -> [String]
splitAtAll _ [] = []
splitAtAll split s = takeWhile (not . split) s : splitAtAll split (dropWhile split $ dropWhile (not . split) s)

-- | Compute a single problem as specified in part 2 after splitting and transposing.
computeRow :: [String] -> Int
computeRow (c:cs) = case reverse c of
    ('*':c') -> product $ fmap read $ reverse c' : cs
    ('+':c') -> sum $ fmap read $ reverse c' : cs

-- | Split a row, for part 2.
splitRows :: [String] -> [[String]]
splitRows [] = []
splitRows rows
    = (takeWhile (not . all isSpace) rows)
    : (splitRows (dropWhile (all isSpace) $ dropWhile (not . all isSpace) rows))

