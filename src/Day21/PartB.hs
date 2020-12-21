import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)

type SetType = S.Set String
type MapType = M.Map String SetType
type MapType' = M.Map String String


parseInput :: [String] -> [String] -> MapType -> MapType
parseInput [] g m = m
parseInput (i:is) g m = let cleaned = filter (\x -> not (x `elem` [',', '(', ')'])) i
                            i_a_split = splitOn " contains " cleaned
                            ingredients = splitOn " " $ head i_a_split
                            allergens = splitOn " " $ i_a_split !! 1
                            updated_m = foldl (\mm a -> let {ex = M.findWithDefault S.empty a mm; merged = S.intersection ex (S.fromList ingredients)} 
                                                            in if length ex == 0 then (M.insert a (S.fromList ingredients) mm) 
                                                                else (M.insert a merged mm)) m allergens
                                in parseInput is (g ++ ingredients) updated_m

process :: MapType -> MapType' -> String
process g i
    | length g == 0 = intercalate "," (M.elems i)
    | otherwise     = process g'' i'
        where fst_elem = head $ filter (\x -> length (snd x) == 1) (zip (M.keys g) (M.elems g))
              (rmk, rmv) = (fst fst_elem, (head $ S.toList $ snd fst_elem))
              i' = M.insert rmk rmv i
              g' = M.delete rmk g
              g'' = M.foldlWithKey (\x' k' v' -> (M.insert k' (v' `S.difference` (S.fromList [rmv])) x')) M.empty g'

main = do
    contents <- readFile "Input.txt"
    print $ (process (parseInput (lines contents) [] M.empty) M.empty)
