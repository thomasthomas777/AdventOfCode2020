import System.IO
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as M

type MapType = M.Map Int Int

prepareInput :: [Int] -> (MapType, Int)
prepareInput xs =   let shifted = (tail xs) ++ [head xs]
                        zip' = zip xs shifted
                            in ((foldl' (\x (a, b) -> M.insert a b x) M.empty zip'), head xs)

determineInsertion :: Int -> [Int] -> Int -> Int
determineInsertion l picked 1 = determineInsertion l picked (l+1)
determineInsertion l picked curr = let dest = (curr-1) in case (dest `elem` picked) of
                                                                    True -> determineInsertion l picked dest
                                                                    False -> dest


move :: (MapType, Int) -> Int -> (MapType, Int)
move (m, iter) _ = let  (Just p1) = M.lookup iter m
                        (Just p2) = M.lookup p1 m
                        (Just p3) = M.lookup p2 m
                        (Just tail_val) = M.lookup p3 m
                        insert' = determineInsertion (length m) [p1, p2, p3] iter
                        (Just insert_val) = M.lookup insert' m
                        m' = M.insert iter tail_val m
                        m'' = M.insert p3 insert_val m'
                        m''' = M.insert insert' p1 m''
                            in (m''', tail_val)
    
-- Using the recursive approach for incrementing the iteration from Part A, caused a stack overflow exception to be raised.
-- To resolve, the strict foldl implementation from an outer function was leveraged to prevent build up of lazy evaluated components - otherwise, it builds up fast with non mutable maps.
iterateMove :: (MapType, Int) -> Int -> Int
iterateMove (m, a) max_iter = let   (m', aa) = foldl' (\x y -> (move x y)) (m, a) [1..(max_iter)]
                                    fst = M.findWithDefault 0 1 m'
                                    snd = M.findWithDefault 0 fst m'
                                        in fst * snd

main = do
    print $ iterateMove (prepareInput ([2, 1, 5, 6, 9, 4, 7, 8, 3] ++ [10..1000000])) 10000000