import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)

type SetType = S.Set String
type MapType = M.Map String SetType


parseInput :: [String] -> [String] -> MapType -> (MapType, [String])
parseInput [] g m = (m, g)
parseInput (i:is) g m = let cleaned = filter (\x -> not (x `elem` [',', '(', ')'])) i
                            i_a_split = splitOn " contains " cleaned
                            ingredients = splitOn " " $ head i_a_split
                            allergens = splitOn " " $ i_a_split !! 1
                            updated_m = foldl (\mm a -> let {ex = M.findWithDefault S.empty a mm; merged = S.intersection ex (S.fromList ingredients)} 
                                                            in if length ex == 0 then (M.insert a (S.fromList ingredients) mm) 
                                                                else (M.insert a merged mm)) m allergens
                                in parseInput is (g ++ ingredients) updated_m

process :: (MapType, [String]) -> Int
process (g, m) = let a = M.foldl (\x y -> x ++ (S.toList y)) [] g in length $ filter (\z -> not $ z `elem` a) m

main = do
    contents <- readFile "Input.txt"
    print $ process $ (parseInput (lines contents) [] M.empty)
