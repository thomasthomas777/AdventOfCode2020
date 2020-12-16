import System.IO
import qualified Data.Map as M
import Data.List (isPrefixOf, intersect, transpose)
import Data.List.Split (splitOn)

mapKeyLookup :: Int -> (M.Map String [Int]) -> [String]
mapKeyLookup v m = M.foldlWithKey (\x key val -> if (v `elem` val) then key:x else x) [] m

createMap :: [[String]] -> (M.Map String [Int]) -> (M.Map String [Int])
createMap [] m = m
createMap (x:xs) m = createMap xs (M.insert k merged m)
    where k = head x
          v1a = read ((splitOn "-" (x !! 1)) !! 0) :: Int
          v1b = read ((splitOn "-" (x !! 1)) !! 1) :: Int
          v2a = read ((splitOn "-" (x !! 2)) !! 0) :: Int
          v2b = read ((splitOn "-" (x !! 2)) !! 1) :: Int
          merged = [v1a..v1b] ++ [v2a..v2b]

splitInput :: String -> ([Int], [[Int]], (M.Map String [Int]))
splitInput i = (my_tickets, near_tickets, map')
    where master_split = splitOn "\n\n" i
          map' = createMap (map (\a -> [a!!0] ++ (splitOn " or " (a!!1))) (map (\a -> splitOn ": " a) (lines (master_split !! 0)))) M.empty
          my_tickets = map (\x -> read x :: Int) (splitOn "," (lines (master_split !! 1) !! 1))
          near_tickets =  map (\z->map (\a-> read a :: Int) (splitOn "," z)) (tail $ lines (master_split !! 2))

drop' :: ([Int], [[Int]], M.Map String [Int]) -> ([Int], [[Int]], M.Map String [Int])
drop' (a, b, m)= let compressed_c = concat (M.foldr (:) [] m) in 
                                            (a, transpose (filter (\x->foldl (\a b -> a && (b `elem` compressed_c)) True x) b), m)

bucket :: ([Int], [[Int]], M.Map String [Int]) -> [(Int, String)] -> Int -> Int
bucket (xs, ys, m) succ c 
    | (length succ == length xs)    = foldl (\x y -> x * (xs !! y)) 1 succ_idxs_depature
    | c `elem` succ_idxs            = bucket (xs, ys, m) succ next_c
    | otherwise                     = bucket (xs, ys, m) new_succ next_c
        where   ys' = foldl (\a b -> (mapKeyLookup b m):a) [] ((xs!!c):(ys!!c))
                intersect' = foldr1 intersect ys'
                valid_keys' = [a | a <- (M.keys m), not (a `elem` succ_vals)]
                intersect'' = intersect intersect' valid_keys'
                new_succ = if length intersect'' == 1 then ((c, (head intersect'')):succ) else succ

                next_c = (c + 1) `mod` (length xs)
                succ_idxs = map fst succ
                succ_vals = map snd succ
                succ_idxs_depature = map fst (filter (\z -> "departure" `isPrefixOf` (snd z)) succ)

main = do
    contents <- readFile "Input.txt"
    print $ bucket (drop' $ splitInput contents) [] 0