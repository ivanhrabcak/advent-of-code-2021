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

    let winningBoardN = getWinningBoardN input drawnNumbers 0

    let losingBoardState = getLosingBoard input drawnNumbers

    let losingBoardWinningState = getWinningBoardState (fst losingBoardState) drawnNumbers 0

    let losingBoardWinningN = getEBeforeElem drawnNumbers (-1) (snd losingBoardWinningState)
    let unmarkedSum = sumUnmarked (fst losingBoardWinningState) 0
    
    print (unmarkedSum * losingBoardWinningN)

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

getWinningBoardN :: [Int] -> [Int] -> Int -> Int
getWinningBoardN boards (x:xs) i
    | isWinningBoard (getBoard boards i) = i
    | i == div (length boards) 25 = getWinningBoardN (filterNumber boards x) xs 0
    | otherwise = getWinningBoardN boards (merge xs [x]) (i + 1)

getNOfWinningBoards :: [Int] -> Int -> Int -> Int
getNOfWinningBoards boards i n
    | i == div (length boards) 25 = n
    | otherwise = getNOfWinningBoards boards (i + 1) (if isWinningBoard (getBoard boards i) then n + 1 else n)

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

filterWinningBoards :: [Int] -> Int -> [Int]
filterWinningBoards boards i
    | i == div (length boards) 25 = boards
    | otherwise = filterWinningBoards (if isWinningBoard (getBoard boards i) then removeBoardN boards i else boards) (if isWinningBoard (getBoard boards i) then 0 else i + 1)

getLosingBoard :: [Int] -> [Int] -> ([Int], Int)
getLosingBoard boards (x:xs)
    | getNOfWinningBoards boards 0 0 == div (length boards) 25 - 1 = (filterWinningBoards boards 0, x)
    | null xs = (boards, -1)
    | otherwise = getLosingBoard (filterNumber boards x) xs

removeBoardN :: [Int] -> Int -> [Int]
removeBoardN boards n = filterIndexed (\x i -> not (i >= n * 25 && i < (n * 25) + 25)) boards

filterIndexed :: (a -> Int -> Bool) -> [a] -> [a]
filterIndexed p xs = [x|(x,i) <- zip xs [0..], p x i]

getBoard :: [Int] -> Int -> [Int]
getBoard input boardN = filterIndexed (\x i -> i >= boardN * 25 && i < (boardN * 25) + 25) input