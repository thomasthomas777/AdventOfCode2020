import System.IO
import Data.List
import qualified Data.Map as M

type Point = (Int, Int, Int)
type PointMap = M.Map Point Bool

move :: String -> Point -> Point
move [] curr_point = curr_point
move (x:xs) curr_point
    | nxt == "e" = let new_point = (lst_pnt_x + 1, lst_pnt_y - 1, lst_pnt_z) in move future new_point
    | nxt == "se" = let new_point = (lst_pnt_x, lst_pnt_y - 1, lst_pnt_z + 1) in move future new_point
    | nxt == "sw" = let new_point = (lst_pnt_x - 1, lst_pnt_y, lst_pnt_z + 1) in move future new_point
    | nxt == "w" = let new_point = (lst_pnt_x - 1, lst_pnt_y + 1, lst_pnt_z) in move future new_point
    | nxt == "nw" = let new_point = (lst_pnt_x, lst_pnt_y + 1, lst_pnt_z - 1) in move future new_point
    | nxt == "ne" = let new_point = (lst_pnt_x + 1, lst_pnt_y, lst_pnt_z - 1) in move future new_point
        where   double' = x `elem` ['n', 's'] 
                nxt = if double' then x:[head xs] else [x]
                future = if double' then tail xs else xs
                (lst_pnt_x, lst_pnt_y, lst_pnt_z) = curr_point

moveTiles :: [String] -> PointMap -> Int
moveTiles [] co = length $ M.filter (==True) co
moveTiles (x:xs) co = let {f = move x (0, 0, 0); co' = M.insert f (not $ M.findWithDefault False f co) co} in moveTiles xs co'

main = do
    contents <- readFile "Input.txt"
    print $ moveTiles (lines contents) M.empty