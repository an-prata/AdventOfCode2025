import Data.Maybe
import Data.List
import Data.MemoTrie
import qualified Data.Map as M
import GHC.Clock

main :: IO ()
main = do
    conns <- M.fromList <$> fmap ((\(a, b) -> (a, words b)) . splitAtChar ':') <$> lines <$> readFile "Day11.txt"

    -- Part 1
    print $ pathsToFrom "out" conns "you"

    -- Part 2
    print
        $ (pathsToFrom "fft" conns "svr") * (pathsToFrom "dac" conns "fft") * (pathsToFrom "out" conns "dac")
        + (pathsToFrom "dac" conns "svr") * (pathsToFrom "fft" conns "dac") * (pathsToFrom "out" conns "fft")

-- | Calculate the number of paths from the first argument, to the third, through the given 'Map'.
pathsToFrom target conns = memoFix $ pathsToFromUnfixed target conns

-- | Backing function for a memoized 'pathsFromTo'
pathsToFromUnfixed :: (HasTrie a, Ord a) => a -> M.Map a [a] -> (a -> Int) -> a -> Int
pathsToFromUnfixed target conns rec start = if start == target
    then 1
    else case conns M.!? start of
        Just nexts -> sum $ fmap rec nexts
        Nothing -> 0

-- | Split a 'String' at the first instance of the given 'Char'.
splitAtChar :: Char -> String -> (String, String)
splitAtChar split s = (takeWhile (/= split) s, drop 1 $ dropWhile (/= split) s)
