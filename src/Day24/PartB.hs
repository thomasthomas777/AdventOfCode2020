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

moveTiles :: [String] -> PointMap -> PointMap
moveTiles [] co = co
moveTiles (x:xs) co = let {f = move x (0, 0, 0); co' = M.insert f (not $ M.findWithDefault False f co) co} in moveTiles xs co'

neighbourTraversal :: PointMap -> PointMap
neighbourTraversal pm = let     neighbourTiles (x', y', z') pm' = foldl (\x (dx, dy, dz) -> let np = (x'+dx, y'+dy, z'+dz) in M.insert np (M.findWithDefault False np pm) x) pm' [(1, -1, 0), (0, -1, 1), (-1, 0, 1), (-1, 1, 0), (0, 1, -1), (1, 0, -1)]
                                allNeighbourTiles = M.foldlWithKey (\x k v -> if v then (neighbourTiles k x) else x) M.empty pm
                                    in M.foldlWithKey (\x k v -> let cnt = length $ M.filter (==True) (neighbourTiles k M.empty) in 
                                                            if v then if (cnt == 0 || cnt > 2) then M.insert k False x else x
                                                            else if cnt == 2 then M.insert k True x else x) allNeighbourTiles allNeighbourTiles
                                
dayIterate :: PointMap -> Int -> Int
dayIterate pm 0 = length $ M.filter (==True) pm
dayIterate pm d = dayIterate (neighbourTraversal pm) (d-1)

main = do
    contents <- readFile "Input.txt"
    print $ dayIterate (moveTiles (lines contents) M.empty) 100