import System.IO
import Data.List
import Data.List.Split
import Data.Set (toList, fromList)


splitAndSetifyInput :: String -> [String]
splitAndSetifyInput x = map (\x -> toList $ fromList ( filter (/= '\n') x) ) (splitOn "\n\n" x)

count :: [String] -> Int
count = foldl (\x y -> (length y + x)) 0

main = do
    contents <- readFile "Input.txt"
    print $ count $ splitAndSetifyInput $ contents
