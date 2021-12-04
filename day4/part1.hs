import System.IO
import Data.List
import Data.Char

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let wholeInput = words contents

    let drawnNumbers = parseDrawnNums wholeInput

    let input = map (\x -> read x::Int) (popFirst wholeInput)

    let winningBoardState = getWinningBoardState input drawnNumbers 0

    -- let board = [-1, -1, -1, 0, -1,
    --              0, 0, 0, 0, -1,
    --              0, -1, 0, 0, -1,
    --              0, -1, 0, 0, -1,
    --              -1, -1, 0, 0, -1]
    -- let board = getBoard input 495
    -- print (isWinningBoard board)
    -- print board
    -- print (length board)

    -- print winningBoardState
    print winningBoardState
    -- print drawnNumbers

    let unmarkedSum = sumUnmarked (fst winningBoardState) 0
    print unmarkedSum
    let lastDrawnN = getEBeforeElem drawnNumbers (-1) (snd winningBoardState)
    print (unmarkedSum * lastDrawnN)

getEBeforeElem :: [Int] -> Int -> Int -> Int
getEBeforeElem (x:xs) previous n
    | x == n = previous
    | otherwise = getEBeforeElem xs x n

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

popFirst :: [String] -> [String]
popFirst (x:xs) = xs

parseDrawnNums :: [String] -> [Int]
parseDrawnNums input = map read (wordsWhen (==',') (head input))

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

sumUnmarked :: [Int] -> Int -> Int
sumUnmarked (x:xs) sum
    | null xs = if x /= -1
                 then sum + x
                else sum
    | otherwise = sumUnmarked xs (if x /= -1 then sum + x else sum)

filterNumber :: [Int] -> Int -> [Int]
filterNumber input n = map (\x -> if x /= n then x else -1) input

getWinningBoardState :: [Int] -> [Int] -> Int -> ([Int], Int)
getWinningBoardState boards (x:xs) i
    | isWinningBoard (getBoard boards i) = (getBoard boards i, head xs)
    | i == div (length boards) 25 = getWinningBoardState (filterNumber boards x) xs 0
    | otherwise = getWinningBoardState boards (merge xs [x]) (i + 1)

isWinningRow :: [Int] -> Bool
isWinningRow row = not (any (/= -1) row)

getRow :: [Int] -> Int -> [Int]
getRow board rowN = filterIndexed (\x i -> i >= rowN * 5 && i < (rowN + 1) * 5) board

divideIntoRows :: [Int] -> Int -> [[Int]] -> [[Int]]
divideIntoRows board i res
    | i == 4 = merge res [getRow board 4]
    | otherwise = divideIntoRows board (i + 1) (merge res [getRow board i])

getColumn :: [Int] -> Int -> [Int]
getColumn board columnN = filterIndexed (\x i -> mod i 5 == columnN) board

divideIntoColumns :: [Int] -> Int -> [[Int]] -> [[Int]]
divideIntoColumns board i res
    | i == 4 = merge res [getColumn board 4]
    | otherwise = divideIntoColumns board (i + 1) (merge res [getColumn board i])

isWinningBoard :: [Int] -> Bool
isWinningBoard board = not (null filteredRows || null (head filteredRows)) || not (null filteredColumns || null (head filteredColumns))
                       where filteredRows = filter isWinningRow (divideIntoRows board 0 [])
                             filteredColumns = filter isWinningRow (divideIntoColumns board 0 [])

filterIndexed :: (a -> Int -> Bool) -> [a] -> [a]
filterIndexed p xs = [x|(x,i) <- zip xs [0..], p x i]

getBoard :: [Int] -> Int -> [Int]
getBoard input boardN = filterIndexed (\x i -> i >= boardN * 25 && i < (boardN * 25) + 25) input
