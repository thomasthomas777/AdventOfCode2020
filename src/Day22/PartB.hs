import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)

type SetType = S.Set String
type MapType = M.Map String SetType

type Deck = ([Int], [Int])

parseInput :: String -> Deck
parseInput l = let  x = splitOn "\n\n" l
                    a = lines (x !! 0)
                    b = lines (x !! 1)
                    a' = map (\x -> read x :: Int) (tail a)
                    b' = map (\x -> read x :: Int) (tail b)
                        in (a',b')

computeWinning :: [Int] -> Int
computeWinning x = sum $ map (\(a,b) -> a * b) (zip (reverse x) [1..])

process :: Deck -> [Deck] -> (Int, Int)
process ([], []) a = error "Equal Draw"
process (x, []) a = (1, computeWinning x)
process ([], y) a = (2, computeWinning y)
process ((x:xs), (y:ys)) a
    | seen          = (1, computeWinning (x:xs))
    | card_gt       = if sub_game_win == 1 then process (xs ++ [x, y], ys) new_deck else process (xs, ys ++ [y, x]) new_deck
    | r             = process (xs ++ [x, y], ys) new_deck
    | r == False    = process (xs, ys ++ [y, x]) new_deck
        where  r = x > y 
               curr_deck = ((x:xs), (y:ys))
               seen =  curr_deck`elem` a
               new_deck = curr_deck:a
               card_gt = (length xs >= x) && (length ys >= y)
               (sub_game_win, _) = process (take x xs, take y ys) []

main = do
    contents <- readFile "Input.txt"
    print $ snd (process (parseInput contents) [])
