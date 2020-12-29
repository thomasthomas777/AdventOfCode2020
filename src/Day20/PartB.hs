import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Function (on)

type Image = [String]

type Combinations = (Int, Image)
type MapType = M.Map (Int, Int) Combinations

combinationsGenerator :: Image -> [Image]
combinationsGenerator i = let   flip_x = reverse i
                                flip_y = transpose $ reverse $ transpose i
                                rotate_90 = reverse $ transpose i
                                rotate_180 = reverse $ transpose rotate_90
                                rotate_270 = reverse $ transpose rotate_180
                                flip_x_rotate_90 = reverse $ transpose flip_x
                                flip_x_rotate_180 = reverse $ transpose flip_x_rotate_90
                                flip_x_rotate_270 = reverse $ transpose flip_x_rotate_180
                                flip_y_rotate_90 = reverse $ transpose flip_y
                                flip_y_rotate_180 = reverse $ transpose flip_y_rotate_90
                                flip_y_rotate_270 = reverse $ transpose flip_y_rotate_180
                                    in S.toList $ S.fromList $ [i, flip_x, flip_y, rotate_90, rotate_180, rotate_270, flip_x_rotate_90, flip_x_rotate_180, flip_x_rotate_270, flip_y_rotate_90, flip_y_rotate_180, flip_y_rotate_270]

parseInput :: [String] -> [Combinations]
parseInput [] = []
parseInput (i:is) = let lined_i = lines i
                        pic_id = read (init $ drop 5 (head lined_i)) :: Int
                        raw_img = tail lined_i
                        all_combinations = combinationsGenerator raw_img
                            in [(pic_id, z) | z <- all_combinations] ++ parseInput is

prepareMapper :: [Combinations] -> ([Combinations], MapType)
prepareMapper is = let n = (floor $ sqrt $ fromIntegral $ ((length is) `quot` 8)) - 1
                                in (is ,foldl (\a b -> M.insert b (0, [[]]) a) M.empty [(y, x) | y <- [0..n], x <- [0..n]])

process :: [(Int, Int)] -> [(Int, Int)] -> ([Combinations], MapType) -> (M.Map (Int, Int) [Combinations]) -> ([Combinations], MapType)
process [] ys (rc, m) d = (rc, m)
process (x:xs) ys (rc, m) d
    | x == (n,n) && (nxt_img /= (0, [""])) = ([], m')
    | (nxt_img /= (0, [""])) && (length rc_curr_iter > 0)= process xs (x:ys) (rc, m') d'
    | otherwise = process revert_point revert_point_ys (rc, m''') d''
        where   n = (floor $ sqrt $ fromIntegral (length m)) - 1
                done_combinations = M.findWithDefault [] x d
                imgs_processed = map (\z -> fst z) (M.elems (M.filter (\x -> fst x /= 0) m))

                rc_curr_iter = [i | i <- rc, not ((fst i) `elem` imgs_processed), not (i `elem` done_combinations)]

                (x', y') = x

                determineImageAtLoc []    = (0, [""])
                determineImageAtLoc (a:b) = let determineImageAtLocInner _ [] = (False, False)
                                                determineImageAtLocInner (s1, s2) (c:d)
                                                    | (x', y') == (0, 0) || (s1, s2) == (True, True) || (l_x && u_y)    = (True, True)
                                                    | l_x                                                               = determineImageAtLocInner (True, s2) d
                                                    | u_y                                                               = determineImageAtLocInner (s1, True) d
                                                    | otherwise                                                         = determineImageAtLocInner (s1, s2) d
                                                        where   l_x = if x' == 0 then True else let (Just img') = M.lookup (x'-1, y') m in [head y | y <- snd a] == [last y | y <- (snd img')]
                                                                u_y = if y' == 0 then True else let (Just img') = M.lookup (x', y'-1) m in (head $ snd a) == (last $ snd img')
                                                    in case determineImageAtLocInner (False, False) rc_curr_iter of 
                                                        (True, True) -> a
                                                        _ -> determineImageAtLoc b

                nxt_img = determineImageAtLoc rc_curr_iter

                m' = M.insert x nxt_img m
                d' = if nxt_img == (0, [""]) then d else M.insert x (nxt_img:done_combinations) d

                m'' = M.insert x (0, [""]) m
                m''' = M.insert (head ys) (0, [""]) m''
                
                d'' = M.insert x [] d

                revert_point = ((head ys):x:xs)
                revert_point_ys = tail ys

cleanImage :: MapType -> Image 
cleanImage m = let  n = (floor $ sqrt $ fromIntegral (length m)) - 1
                    o = length $ head $ snd $ (M.findWithDefault (0, [""]) (0, 0) m)
                    stripBorders i = map (\z -> (tail (init z))) (tail (init (snd i)))
                    zipImg (x,y) x' = zipWith (++) x' (stripBorders $ (M.findWithDefault (0, [""]) (x, y) m))
                        in foldl (\y' y -> y' ++ (foldl (\x' x -> zipImg (x, y) x') (take o (repeat "")) ([0..n]))) [] [0..n]

findDragon :: Image -> Int
findDragon i = let  allI = combinationsGenerator i
                    totalTiles = sum $ map (\x -> length $ filter (=='#') x) i
                    dragonTiles = ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]
                    totalDragonTiles = sum $ map (\x -> length $ filter (=='#') x) dragonTiles
                    (dlx, dly) = (length $ head dragonTiles, length dragonTiles)
                    dragonTilesCompressed = intercalate "" dragonTiles
                    dragonTilesCompressedLength = length dragonTilesCompressed
                    dragonExists i' dx dy = let r_i = intercalate "" (map (\x -> (take dlx (drop dx x))) (take dly (drop dy i')))
                                                in sum $ map (\(a,b) -> if a == '#' then if b == '#' then 1 else 0 else 1) (zip dragonTilesCompressed r_i)
                    allCoords = [(x', y') | x' <- [0..(length (head i) - dlx)], y' <- [0..(length i - dly)]]
                    checkAllCoords i' = foldl (\a (dx, dy) -> if dragonExists i' dx dy == dragonTilesCompressedLength then a+totalDragonTiles else a) 0 allCoords
                        in totalTiles - foldl (\a i' -> a + checkAllCoords i') 0 allI

processBacktrack :: ([Combinations], MapType) -> Int
processBacktrack (c, m) = let   n = (floor $ sqrt $ fromIntegral (length m)) - 1
                                complete_grid = snd $ process (sortBy (compare `on` snd) (M.keys m)) [] (c, m) M.empty
                                    in findDragon (cleanImage complete_grid)
        
main = do
    contents <- readFile "Input.txt"
    print $ processBacktrack (prepareMapper (parseInput (splitOn "\n\n" contents)))
