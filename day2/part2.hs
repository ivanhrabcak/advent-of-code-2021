import System.IO 
import Data.List

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = words contents

    let output = part2 input "" 0 0 0 0

    print (uncurry (*) output)

part2 :: [String] -> String -> Int -> Int -> Int -> Int -> (Int, Int)
part2 (x:xs) previous i depth travelled aim
    | null xs = if "forward" == previous 
                 then (depth + aim * read x::Int, travelled + read x::Int)
                else (depth, travelled)
    | i == 0 = part2 xs x (i + 1) depth travelled aim
    | mod i 2 == 1 = if "forward" == previous 
                    then part2 xs x (i + 1) (depth + aim * read x::Int) (travelled + read x::Int) aim
                   else if previous == "up"
                    then part2 xs x (i + 1) depth travelled (aim - read x::Int)
                   else if previous == "down"
                    then part2 xs x (i + 1) depth travelled (aim + read x::Int)
                   else (depth, travelled)
    | otherwise = part2 xs x (i + 1) depth travelled aim