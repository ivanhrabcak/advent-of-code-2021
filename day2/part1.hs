import System.IO 
import Data.List

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = words contents

    let output = part1 input "" 0 0 0

    print (uncurry (*) output)

part1 :: [String] -> String -> Int -> Int -> Int -> (Int, Int)
part1 (x:xs) previous i depth travelled
    | null xs = if "forward" == previous 
                 then (depth, travelled + read x::Int)
                else if previous == "up"
                 then (depth - read x::Int, travelled)
                else if previous == "down"
                 then (depth + read x::Int, travelled)
                else (depth, travelled)
    | i == 0 = part1 xs x (i + 1) depth travelled
    | mod i 2 == 1 = if "forward" == previous 
                    then part1 xs x (i + 1) depth (travelled + read x::Int)
                   else if previous == "up"
                    then part1 xs x (i + 1) (depth - read x::Int) travelled
                   else if previous == "down"
                    then part1 xs x (i + 1) (depth + read x::Int) travelled
                   else (depth, travelled)
    | otherwise = part1 xs x (i + 1) depth travelled