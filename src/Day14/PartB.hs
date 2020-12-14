import System.IO
import qualified Data.Map as M
import Data.List.Split (splitOn)

dec2bin :: Int -> String
dec2bin 0 = "0"
dec2bin n = dec2bin (n `div` 2) ++ (show (n `mod` 2))

bin2dec :: String -> Int
bin2dec xs = foldl1 (\x y -> 2*x+y) [(read [x] :: Int) | x <- xs] 

applyMask :: [(Char, Char)] -> [String] -> [Int]
applyMask [] der = map bin2dec der
applyMask ((a,b):xs) der = case b of 
                            'X' -> applyMask xs [d++[r] | d <- der, r <- ['0', '1']]
                            '0' -> applyMask xs (map (\x -> x++[a]) der)
                            '1' -> applyMask xs (map (\x -> x++[b]) der)

updateMap :: [Int] -> Int -> (M.Map Int Int) -> (M.Map Int Int)
updateMap [] val reg = reg
updateMap (x:xs) val reg = updateMap xs val (M.insert x val reg)

process :: [String] -> String -> (M.Map Int Int) -> Int
process [] mask reg = M.foldl' (+) 0 reg
process (x:xs) mask reg 
    | instruction == "mask" =   process xs value reg
    | otherwise =               process xs mask (updateMap computed value_as_int reg)
        where splitted = splitOn " = " x
              instruction = head splitted
              instruction_loc = read (init (drop 4 instruction)) :: Int
              value = splitted !! 1
              value_as_int = read value :: Int
              instruction_to_bin = dec2bin instruction_loc
              mask_length = length mask
              instruction_length = length instruction_to_bin
              instruction_to_bin_filled = (replicate (mask_length - instruction_length) '0') ++ instruction_to_bin
              computed = applyMask (zip instruction_to_bin_filled mask) [""]

main = do
    contents <- readFile "Input.txt"
    print $ process (lines contents) "" M.empty
