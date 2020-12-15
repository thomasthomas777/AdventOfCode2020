import qualified Data.Map as M

process :: [Int] -> Int -> Int -> (M.Map Int Int) -> Int
process (x:[]) p c m = process [] x (c+1) m
process (x:xs) p c m = process xs x (c+1) (M.insert x c m)
process [] p 30000000 m = p
process [] p c m = let lkup = (M.lookup p m) in case lkup  of 
                                                    Just x -> process [] ((c-1)-x) (c+1) (M.insert p (c-1) m)
                                                    Nothing -> process [] 0 (c+1) (M.insert p (c-1) m)

main = do
    print $ process [0, 1, 5, 10, 3, 12, 19] 0 0 M.empty
