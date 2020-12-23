import System.IO
import Data.List
import Data.Maybe (fromJust)
import Data.Char (intToDigit)

determineInsertion :: [Int] -> [Int] -> Int -> Int -> Int
determineInsertion picked rest curr alt'
    | exists_picked || not_exists_rest = determineInsertion picked rest dest alt'
    | otherwise = dest
        where   max_elem = foldl1 max (alt':rest)
                dest = (curr - 1) `mod` (max_elem+1)
                exists_picked = dest `elem` picked
                not_exists_rest = not (dest `elem` rest)

iterateMove :: [Int] -> Int -> Int -> String
iterateMove xs iter max_iter
    | iter == max_iter  = let idx = fromJust $ elemIndex 1 xs in map intToDigit ((drop (idx+1) xs) ++ (take (idx) xs))
    | otherwise         = iterateMove new_xs (iter+1) max_iter
                            where   curr = xs !! 0
                                    picked = take 3 (drop (1) xs)
                                    rest = drop 4 xs
                                    insert' = determineInsertion picked rest curr curr
                                    insert_idx = fromJust $ elemIndex insert' rest
                                    new_xs = (take (insert_idx + 1) rest) ++ picked ++ (drop (insert_idx + 1) rest) ++ [curr]
                    
main = do
    print $ iterateMove [2, 1, 5, 6, 9, 4, 7, 8, 3] 0 100