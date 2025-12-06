import Data.Maybe
import Data.List

main :: IO ()
main = do
    grid <- lines <$> readFile "Day4.txt"

    -- Part 1 
    print $ length $ filter (== 'x') $ concat $ accessRolls grid

    -- Part 2
    print $ maxAccessable grid

-- | Access the given grid multiple times until no more rolls can be accessed, and return the total
-- number of rolls which were accessed.
maxAccessable :: [[Char]] -> Int
maxAccessable grid = case numAccessed of
    0 -> 0
    n -> n + maxAccessable accessed
  where
    accessed = fmap (fmap (\c -> if c == 'x' then '.' else c)) $ accessRolls grid
    numAccessed = sum $ fmap (length . filter (== 'x')) $ accessRolls grid

-- | Replaces all accessable roll ('@') values with ('x') in the given grid.
accessRolls :: [[Char]] -> [[Char]]
accessRolls grid = fmap
    (\row -> fmap
        (replaceGridItem row)
        [0..length (grid !! row) - 1]
    )
    [0..length grid - 1]
  where
    replaceGridItem row column = case grid !! row !! column of
        '@' -> if posIsAccessable grid row column then 'x' else '@'
        item -> item

-- | Returns true if the given (row, column) position would be accessable within the given grid.
posIsAccessable :: [[Char]] -> Int -> Int -> Bool
posIsAccessable grid row column = (4 >)
    $ sum
    $ fmap (length . filter (== '@'))
    $ fmap maybeToList
        [ grid !? (row + 1) >>= (!? (column + 1))
        , grid !? (row + 1) >>= (!?  column     )
        , grid !? (row + 1) >>= (!? (column - 1))
        , grid !?  row      >>= (!? (column + 1))
        , grid !?  row      >>= (!? (column - 1))
        , grid !? (row - 1) >>= (!? (column + 1))
        , grid !? (row - 1) >>= (!?  column     )
        , grid !? (row - 1) >>= (!? (column - 1))
        ]
