import Data.Char

main :: IO ()
main = do
    ids <- concat
        <$> fmap (idRange . splitAtAll '-')
        <$> splitAtAll ','
        <$> filter (not . isSpace)
        <$> readFile "Day2.txt"

    -- Part 1
    print $ sum $ read <$> filter (not . isValidIdP1) ids

    -- Part 2
    print $ sum $ read <$> filter (not . isValidIdP2) ids

-- | Returns `True` if `id` is a valid ID (by part two).
isValidIdP2 :: Eq a => [a] -> Bool
isValidIdP2 id = and $ fmap (not . (`isRepetitionOf` id) . (`take` id)) [1..length id `div` 2]

-- | Returns `True` if `id` is a valid ID (by part two).
isValidIdP1 :: Eq a => [a] -> Bool
isValidIdP1 id = not $ isRepetitionOf (take (length id `div` 2) id) id

-- | True if `test` is built by repeating `seq` some number of times.
isRepetitionOf :: Eq a => [a] -> [a] -> Bool
isRepetitionOf [] _ = False
isRepetitionOf _ [] = True
isRepetitionOf seq test | length test `mod` length seq /= 0 = False
isRepetitionOf seq test = take (length seq) test == seq && isRepetitionOf seq (drop (length seq) test)

-- | Creates a list of IDs from the given start and end.
idRange :: [String] -> [String]
idRange [start, end] = show <$> [start' .. end']
  where
    start' = read start :: Int
    end' = read end :: Int

-- | Split a `String` at all occurances of the given `Char`.
splitAtAll :: Char -> String -> [String]
splitAtAll _ [] = []
splitAtAll split s = takeWhile (/= split) s : splitAtAll split (drop 1 $ dropWhile (/= split) s)
