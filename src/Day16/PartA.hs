import System.IO
import qualified Data.Map as M
import Data.List.Split (splitOn)

createMap :: [[String]] -> (M.Map String [Int]) -> (M.Map String [Int])
createMap [] m = m
createMap (x:xs) m = createMap xs (M.insert k merged m)
    where k = head x
          v1a = read ((splitOn "-" (x !! 1)) !! 0) :: Int
          v1b = read ((splitOn "-" (x !! 1)) !! 1) :: Int
          v2a = read ((splitOn "-" (x !! 2)) !! 0) :: Int
          v2b = read ((splitOn "-" (x !! 2)) !! 1) :: Int
          merged = [v1a..v1b] ++ [v2a..v2b]

splitInput :: String -> ([Int], [Int], (M.Map String [Int]))
splitInput i = (my_tickets, near_tickets, map')
    where master_split = splitOn "\n\n" i
          map' = createMap (map (\a -> [a!!0] ++ (splitOn " or " (a!!1))) (map (\a -> splitOn ": " a) (lines (master_split !! 0)))) M.empty
          my_tickets = map (\x -> read x :: Int) (splitOn "," (lines (master_split !! 1) !! 1))
          near_tickets = concat $ map (\z->map (\a-> read a :: Int) (splitOn "," z)) (tail $ lines (master_split !! 2))

process :: ([Int], [Int], M.Map String [Int]) -> Int
process (a, b, m) = let compressed_c = concat (M.foldr (:) [] m) in 
                                            foldl (\x y -> (if y `elem` compressed_c then 0 else y) + x) 0 b


main = do
    contents <- readFile "Input.txt"
    print $ process $ splitInput contents
