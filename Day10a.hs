import System.IO
import Data.List

calc :: [Int] -> Int -> Int -> Int -> Int
calc [] p j1 j3 = j1 * (j3 + 1)
calc (x:xs) p j1 j3 
    | ((x - 1) == p) = calc xs x (j1+1) j3
    | ((x - 3) == p) = calc xs x j1 (j3+1)

main = do
    contents <- readFile "Day10Data.txt"
    print $ calc (sort $ map (\x -> read x :: Int) (lines contents)) 0 0 0
