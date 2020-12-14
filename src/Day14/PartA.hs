import System.IO
import qualified Data.Map as M
import Data.List.Split (splitOn)

dec2bin :: Int -> String
dec2bin 0 = "0"
dec2bin n = dec2bin (n `div` 2) ++ (show (n `mod` 2))

bin2dec :: String -> Int
bin2dec xs = foldl1 (\x y -> 2*x+y) [(read [x] :: Int) | x <- xs] 

applyMask :: [(Char, Char)] -> String -> Int
applyMask [] der = bin2dec der
applyMask ((a,b):xs) der = case b of 
                            'X' -> applyMask xs (der ++ [a])
                            '0' -> applyMask xs (der ++ [b])
                            '1' -> applyMask xs (der ++ [b])

process :: [String] -> String -> (M.Map Int Int) -> Int
process [] mask reg = M.foldl' (+) 0 reg
process (x:xs) mask reg 
    | instruction == "mask" =   process xs value reg
    | otherwise =               process xs mask (M.insert instruction_loc computed reg)
        where splitted = splitOn " = " x
              instruction = head splitted
              instruction_loc = read (init (drop 4 instruction)) :: Int
              value = splitted !! 1
              value_as_int = read value :: Int
              value_to_bin = dec2bin value_as_int
              mask_length = length mask
              value_length = length value_to_bin
              value_to_bin_filled = (replicate (mask_length - value_length) '0') ++ value_to_bin
              computed = applyMask (zip value_to_bin_filled mask) ""



main = do
    contents <- readFile "Input.txt"
    print $ process (lines contents) "" M.empty
