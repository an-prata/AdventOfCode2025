import Data.Maybe
import Data.List

-- | Inclusive range between two `Int`s.
data Range = Range Int Int
    deriving Eq

main :: IO ()
main = do
    (rangesStr, idsStr) <- splitAtElem "" <$> lines <$> readFile "Day5.txt"

    let ranges = fmap readRange rangesStr
    let ids = fmap read idsStr :: [Int]

    -- Part 1
    print $ length $ filter (isInAnyRange ranges) ids

    -- Part 2
    print $ sum $ fmap rangeSpan $ mergeRangesMax ranges

-- | Merge as many `Range`s as possible until no more `Range`s can be merged.
mergeRangesMax :: [Range] -> [Range]
mergeRangesMax [] = []
mergeRangesMax (r:rs) = case find (isJust . mergeRanges r) rs of
    Just merge -> case mergeRanges r merge of
        Just r' -> mergeRangesMax $ r' : (delete merge rs)
    Nothing -> r : mergeRangesMax rs

-- | Merge two `Range`s into a single `Range` spanning their combined values if they overlap.
mergeRanges :: Range -> Range -> Maybe Range
mergeRanges r1@(Range a b) r2@(Range c d) = if isInRange r2 a || isInRange r2 b || isInRange r1 c || isInRange r1 d
    then Just $ Range (a `min` c) (b `max` d)
    else Nothing

rangeSpan :: Range -> Int
rangeSpan (Range a b) = b - a + 1

-- | `True` is the `Int` is in range of any of the given `Range`.
isInAnyRange :: [Range] -> Int -> Bool
isInAnyRange rs i = any (`isInRange` i) rs

-- | `True` if the given `Int` is within the given `Range`.
isInRange :: Range -> Int -> Bool
isInRange (Range a b) i = i >= a && i <= b

-- | Read a `Range` value in the form `a-b` where `a` and `b` are non-negative integers.
readRange :: String -> Range
readRange s = Range (read a) (read b)
  where
    (a, b) = splitAtElem '-' s

splitAtElem :: Eq a => a -> [a] -> ([a], [a])
splitAtElem split xs = (takeWhile (/= split) xs, drop 1 $ dropWhile (/= split) xs)
