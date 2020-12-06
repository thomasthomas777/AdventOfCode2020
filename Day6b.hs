import System.IO
import Data.List
import Data.List.Split (splitOn)
import Data.Set (toList, fromList)


splitAndSetifyInput :: String -> [[String]]
splitAndSetifyInput x = map (\y -> splitOn "\n" y) (splitOn "\n\n" x)

count :: [[String]] -> Int
count = foldl (\x y -> (length $ foldr1 intersect y) + x) 0

main = do
    contents <- readFile "Day6Data.txt"
    print $ count $ splitAndSetifyInput contents
