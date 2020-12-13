import System.IO
import Data.List.Split (splitOn)

process :: Int -> Int -> [(Int, Int)] -> Int
process t d is
    | (length match == length is)   = t
    | match == []                   = process (t+d) d is
    | otherwise                     = process (t+n_d) n_d is
        where match = [fst i | i <- is, (t + (snd i)) `mod` (fst i) == 0]
              n_d = foldl1 lcm match

main = do
    contents <- readFile "Input.txt"
    print $ process 1 1 $ map (\x -> ((read (fst x) :: Int), snd x)) (filter (\x -> fst x /= "x") (zip (splitOn "," ((lines contents) !! 1)) [0..]))
