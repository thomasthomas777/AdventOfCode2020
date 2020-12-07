import System.IO
import Data.List
import Data.List.Split
import Data.Set (toList, fromList)
import Text.Regex.Posix

data BagDependency = BagDependency String [(String, Int)]

parseLine :: String -> String -> [(String, Int)] -> BagDependency
parseLine s m r
    | init_str /= []  =             parseLine (init_str !! 0 !! 2) ((init_str !! 0) !! 1) []
    | empty_bag /= [] =             BagDependency m r
    | dependency_with_carry /= [] = parseLine (dependency_with_carry !! 0 !! 4) m new_r
    | dependency_no_carry /= [] =   BagDependency m new_rr
        where init_str = s =~ "^([a-z]+ [a-z]+) bags contain (.*)" :: [[String]]
              empty_bag = s =~ "no other bags." :: [[String]]
              dependency_with_carry = s =~ "([0-9]*) ([a-z]+ [a-z]+) (bag|bags), (.*)" :: [[String]]
              dependency_no_carry = s =~ "([0-9]*) ([a-z]+ [a-z]+) (bag|bags)." :: [[String]]
              new_r = (r ++ [((dependency_with_carry !! 0) !! 2, (read ((dependency_with_carry !! 0) !! 1) :: Int))])
              new_rr = (r ++ [((dependency_no_carry !! 0) !! 2, (read ((dependency_no_carry !! 0) !! 1) :: Int))])

colourInsideBag :: String -> BagDependency -> [String]
colourInsideBag c (BagDependency bc co) 
    | c `elem` a = [bc]
    | otherwise  = []
         where a = [fst z | z <- co]

countColor :: String -> [BagDependency] -> [String]
countColor s xs =  foldl (\x y -> (colourInsideBag s y) ++ x) [] xs

countDependencies :: [String] -> [String] -> [BagDependency] -> Int
countDependencies [] b xss = length $ toList $ fromList b
countDependencies (x:xs) b xss = let dep = countColor x xss
                                    in countDependencies (xs ++ dep) (b ++ dep) xss


main = do
    contents <- readFile "Day7Data.txt"
    print $ countDependencies ["shiny gold"] [] (map (\x -> parseLine x "" []) (lines contents))