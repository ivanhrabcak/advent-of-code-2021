import System.IO 
import Data.List
import Data.Char

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = words contents
    print (generatorRating input 0 * scrubberRating input 0)


getBitOnIndex :: String -> Int -> Int
getBitOnIndex l i = read [l !! i]

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

countOnesOnIndex :: [String] -> Int -> Int -> Int
countOnesOnIndex (x:xs) index counter
    | null xs = counter + getBitOnIndex x index
    | otherwise = countOnesOnIndex xs index (counter + getBitOnIndex x index)

mostCommonBitOnIndex :: [String] -> Int -> String
mostCommonBitOnIndex input index
    | count >= length input - count = "1"
    | otherwise = "0"
    where count = countOnesOnIndex input index 0

leastCommonBitOnIndex :: [String] -> Int -> String
leastCommonBitOnIndex input index
    | count >= length input - count = "0"
    | otherwise = "1"
    where count = countOnesOnIndex input index 0

getNumbersWithBitOnIndex :: [String] -> Int -> String -> [String]
getNumbersWithBitOnIndex input constraintIndex bit = filter (\x -> show (getBitOnIndex x constraintIndex) == bit) input

generatorRating :: [String] -> Int -> Int
generatorRating input i
    | i == length (head input) = toDec (head input)
    | length input == 1 = toDec (head input)
    | otherwise = generatorRating (getNumbersWithBitOnIndex input i (mostCommonBitOnIndex input i)) (i + 1)

scrubberRating :: [String] -> Int -> Int
scrubberRating input i
    | i == length (head input) = toDec (head input)
    | length input == 1 = toDec (head input)
    | otherwise = scrubberRating (getNumbersWithBitOnIndex input i (leastCommonBitOnIndex input i)) (i + 1)
    