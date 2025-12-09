import Data.MemoTrie
import Prelude hiding (Left, Right)

data Choice = Left | Right

main :: IO ()
main = do
    input <- lines <$> readFile "Day7.txt"

    -- Part 1
    print $ netSplits input

    -- Part 2
    print $ netTimelines input

netTimelines' = memo netTimelines
stepRowChoices' = memo stepRowChoices

netTimelines :: [String] -> Int
netTimelines (upper:lower:rows) = if choiceMade
    else netTimelines' (lower'Left : rows) + netTimelines' (lower'Right : rows)
    else netTimelines (lower'Left : rows)
  where
    (lower'Left, lower'Right, choiceMade) = stepRowChoices' upper lower
netTimelines _ = 1

runPaths :: [String] -> [String]
runPaths (upper:lower:rows) = lower' : runPaths (lower':rows)
  where
    lower' = stepRow upper lower 
runPaths _ = []

netSplits :: [String] -> Int
netSplits (row:rows) = fst $ foldl stepFun (0, row) rows
  where
    stepFun (net, curr) r =
        let stepped = stepRow curr r
            splits = stepSplits curr r
        in (net + splits, stepped)

stepSplits :: String -> String -> Int
stepSplits ('|':as) ('^':bs) = 1 + stepSplits as bs
stepSplits (_:as) (_:bs) = stepSplits as bs
stepSplits "" "" = 0

-- | Performs a more optimal version of `(stepRowChoose Left prev curr, stepRowChoose Right prev curr)`,
-- in which recursion terminates once the "beam" is found, as this function should only be used for
-- inputs with a single "beam". The `Bool` indicates whether a choice was actually made, or if both
-- stepped rows are the same.
stepRowChoices :: String -> String -> (String, String, Bool)
stepRowChoices (_:'|':_:os) ('.':'^':'.':cs) = ('|' : '^' : '.' : cs , '.' : '^' : '|' : cs, True)
stepRowChoices (_:'|':os)   ('.':'^':cs)     = ('|' : '^' : cs, '.' : '^' : cs, True)
stepRowChoices ('|':_:os)   ('^':'.':cs)     = ('^' : '.' : cs, '^' : '|' : cs, True) 
stepRowChoices ('S':os)     ('.':cs)         = ('|' : cs, '|' : cs, False)
stepRowChoices ('|':os)     ('.':cs)         = ('|' : cs, '|' : cs, False)
stepRowChoices (_:os)       (c:cs)           = (c : l, c : r, b)
  where
    (l, r, b) = stepRowChoices os cs
stepRowChoices "" "" = ("", "", False)

-- | Like `stepRow`, but instead of splitting the "beam", "splitters" are taken to be a "choice", in
-- which the "beam" may go either `Left` or `Right`. This choice is determined by the first
-- parameter of this function.
stepRowChoose :: Choice -> String -> String -> String
stepRowChoose choice@Left  (_:'|':_:os) ('.':'^':'.':cs) = '|' : '^' : '.' : stepRowChoose choice os cs
stepRowChoose choice@Right (_:'|':_:os) ('.':'^':'.':cs) = '.' : '^' : '|' : stepRowChoose choice os cs
stepRowChoose choice@Left  (_:'|':os)   ('.':'^':cs)     = '|' : '^' : stepRowChoose choice os cs
stepRowChoose choice@Right (_:'|':os)   ('.':'^':cs)     = '.' : '^' : stepRowChoose choice os cs
stepRowChoose choice@Left  ('|':_:os)   ('^':'.':cs)     = '^' : '.' : stepRowChoose choice os cs
stepRowChoose choice@Right ('|':_:os)   ('^':'.':cs)     = '^' : '|' : stepRowChoose choice os cs
stepRowChoose choice       ('S':os)     ('.':cs)         = '|' : stepRowChoose choice os cs
stepRowChoose choice       ('|':os)     ('.':cs)         = '|' : stepRowChoose choice os cs
stepRowChoose choice       (_:os)       (c:cs)           = c : stepRowChoose choice os cs
stepRowChoose choice "" "" = ""

-- | Step a row given two rows, first the one above the row we are concerned with, and second the
-- one below. The first row will be the source of any "beams" which make it to the second, and the
-- second will perform any spitting of that "beam".
stepRow :: String -> String -> String
stepRow (_:'|':_:os) ('.':'^':'.':cs) = '|' : '^' : '|' : stepRow os cs
stepRow (_:'|':os)   ('.':'^':cs) = '|' : '^' : stepRow os cs
stepRow ('|':_:os)   ('^':'.':cs) = '^' : '|' : stepRow os cs
stepRow ('S':os)     ('.':cs) = '|' : stepRow os cs
stepRow ('|':os)     ('.':cs) = '|' : stepRow os cs
stepRow (_:os) (c:cs) = c : stepRow os cs
stepRow "" "" = ""
