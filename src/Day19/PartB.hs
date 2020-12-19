import System.IO
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)

type MapType = M.Map Int [[String]]
type SetType = S.Set Int

parseInput :: [String] -> MapType -> (MapType, [String])
parseInput ("":is) m = (m, is)
parseInput (i:is) m = let keySplit = splitOn ": " i
                          key = read (head keySplit) :: Int
                          subList = splitOn " | " (last keySplit)
                          comp = [splitOn " " x | x <- subList]
                          val = if '"' `elem` (last keySplit) then [[[(last keySplit !! 1)]]] else comp
                            in parseInput is (M.insert key val m)


refreshInput :: (MapType, [String]) -> (MapType, [String])
refreshInput (r, c) = (M.insert 8 [["42"], ["42", "8"]] (M.insert 11 [["42", "31"], ["42", "11", "31"]] r), c)

processCombination :: String -> Int -> MapType -> SetType
processCombination m c r
    | length m == 0 =   S.empty
    | isChar        =   if (head $ head $ head $ currRule) == (head m) then (S.fromList [1]) else S.empty 
    | otherwise     =   foldl 
                            (\x xx -> S.union x (foldl 
                                (\y yy -> foldl 
                                    (\z n -> S.union z (S.map (\zz -> zz + n) (processCombination (drop n m) (read yy :: Int) r))) 
                                    S.empty 
                                    y) 
                                (S.fromList [0]) 
                                xx)) 
                            S.empty 
                        currRule
        where   currRule = M.findWithDefault [[]] c r
                isChar = (head $ head $ currRule) `elem` ["a", "b"]

match :: (MapType, [String]) -> Int
match (r, c) = length $ filter (==True) (map (\x -> length x `elem` (S.elems $ processCombination x 0 r)) c)

main = do
    contents <- readFile "Input.txt"
    print $ match $ refreshInput $ parseInput (lines contents) M.empty
