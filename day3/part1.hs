import System.IO 
import Data.List
import Data.Char

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = words contents

    -- let output = part1 input
    -- print (leastCommonBitOnIndex input 2)
    -- print (countOnesOnIndex input 2 0)
    print (getEpsilonRate input 0 "" * getGamma input 0 "")

getBitOnIndex :: String -> Int -> Int
getBitOnIndex l i = read [l !! i]

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

countOnesOnIndex :: [String] -> Int -> Int -> Int
countOnesOnIndex (x:xs) index counter
    | null xs = counter
    | otherwise = countOnesOnIndex xs index (counter + getBitOnIndex x index)

mostCommonBitOnIndex :: [String] -> Int -> String
mostCommonBitOnIndex input index
    | count > length input - count = "1"
    | otherwise = "0"
    where count = countOnesOnIndex input index 0

leastCommonBitOnIndex :: [String] -> Int -> String
leastCommonBitOnIndex input index
    | count > length input - count = "0"
    | otherwise = "1"
    where count = countOnesOnIndex input index 0
    

-- mostCommonBitOnIndex :: [String] -> Int -> String
-- mostCommonBitOnIndex input index
--     | count > div (length input) 2 = "1"
--     | otherwise = "0"
--     where count = countOnesOnIndex input index 0

-- leastCommonBitOnIndex :: [String] -> Int -> String
-- leastCommonBitOnIndex input index
--     | count > div (length input) 2 = "0"
--     | otherwise = "1"
--     where count = countOnesOnIndex input index 0

getGamma :: [String] -> Int -> String -> Int
getGamma input i n
    | i == length (head input) = toDec n
    | otherwise = getGamma input (i + 1) (n ++ mostCommonBitOnIndex input i)

getEpsilonRate :: [String] -> Int -> String -> Int
getEpsilonRate input i n
    | i == length (head input) = toDec n
    | otherwise = getEpsilonRate input (i + 1) (n ++ leastCommonBitOnIndex input i)