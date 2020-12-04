import System.IO
import Data.Char
import Data.List
import Data.List.Split
import Text.Regex.Posix

verifyElement :: String -> Bool
verifyElement x
    | z == "byr" = (m =~ ("^[0-9]{4}$") :: Bool) && a >= 1920 && a <= 2002
    | z == "iyr" = (m =~ ("^[0-9]{4}$") :: Bool) && a >= 2010 && a <= 2020
    | z == "eyr" = (m =~ ("^[0-9]{4}$") :: Bool) && a >= 2020 && a <= 2030
    | z == "hgt" = (h == "cm" && h1 >= 150 && h1 <= 193) || (h == "in" && h1 >= 59 && h1 <= 76)
    | z == "hcl" = m =~ "^#[0-9a-z]{6}$" :: Bool
    | z == "ecl" = m =~ "^(amb|blu|brn|gry|grn|hzl|oth)$" :: Bool
    | z == "pid" = m =~ "^[0-9]{9}$" :: Bool
    | otherwise  = False
    where z = take 3 x 
          m = splitOn ":" x !! 1
          a = read m :: Int
          h = reverse $ take 2 $ reverse x
          h1 = read (take (length m - 2) m) :: Int

verifyPassport :: String -> Bool
verifyPassport s = (length $ filter (\x -> x == True) (map verifyElement $ words s)) == 7

verifyPassports :: [String] -> Int
verifyPassports xs = length $ filter verifyPassport xs

cleanLines :: [String] -> [String]
cleanLines xs =  map (\x -> intercalate " " x) (splitOn [""] xs)

main = do
    contents <- readFile "Day4Data.txt"
    print ( verifyPassports (cleanLines ( lines ( contents ) )))
