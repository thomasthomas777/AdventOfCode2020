import System.IO
import Data.List
import Data.List.Split

verifyElement :: String -> Bool
verifyElement x
    | z == "byr" = True
    | z == "iyr" = True
    | z == "eyr" = True
    | z == "hgt" = True
    | z == "hcl" = True
    | z == "ecl" = True
    | z == "pid" = True
    | otherwise  = False
    where z = take 3 x 

verifyPassport :: String -> Bool
verifyPassport s = (length $ filter (\x -> x == True) (map verifyElement $ words s)) == 7

verifyPassports :: [String] -> Int
verifyPassports xs = length $ filter verifyPassport xs

cleanLines :: [String] -> [String]
cleanLines xs =  map (\x -> intercalate " " x) (splitOn [""] xs)

main = do
    contents <- readFile "Day4Data.txt"
    print ( verifyPassports (cleanLines ( lines ( contents ) )))
