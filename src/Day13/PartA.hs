import System.IO
import Data.List.Split (splitOn)

process :: Int -> [Int] -> Int
process a b = (\x -> fst x * snd x) $ foldl1 (\x y -> if (snd y) < (snd x) then y else x) $ map (\x-> (x, x - (a `mod` x))) b

main = do
    contents <- readFile "Input.txt"
    print $ process (read (head $ lines contents) :: Int) (map (\x->read x :: Int) (filter (/="x") (splitOn "," ((lines contents) !! 1))))
