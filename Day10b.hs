import System.IO
import qualified Data.Map as M
import Data.List

calc :: [Int] -> Int -> (M.Map Int Int) -> Int
calc [] p m = M.findWithDefault 0 p m   -- No need to consider last element as flat in number of ways derivable (last m + 3 only and always)
calc (x:xs) p m = calc xs x new_map
                    where 
                        a1 = M.findWithDefault 0 (x-1) m
                        a2 = M.findWithDefault 0 (x-2) m
                        a3 = M.findWithDefault 0 (x-3) m
                        sum' = a1 + a2 + a3
                        new_map = M.insert x sum' m

main = do
    contents <- readFile "Day10Data.txt"
    print $ calc (sort $ map (\x -> read x :: Int) (lines contents)) 0 (M.fromList [(0, 1)])
