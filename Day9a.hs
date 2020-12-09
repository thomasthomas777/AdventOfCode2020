import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Set (toList, fromList)

isValid :: [Int] -> Int -> Bool
isValid xs c = (length $ filter (==c) [a + b | a <- xs, b <- xs]) > 0
        
count :: [Int] -> Int -> Int
count xs x
    | z == False = currVal
    | otherwise  = count (tail xs) x
        where   priorList = take x xs
                currVal = xs !! x
                z = isValid priorList currVal

main = do
    contents <- readFile "Day9Data.txt"
    print $ count (map (\x -> read x :: Int) (lines contents)) 25
