
{-
 - DAY 2 | Puzzle 1 | Star 3/24
-}

exampleInput :: [[Int]]
exampleInput = [[{- Redacted -}]]

actualInput :: [[Int]]
actualInput = [[{- Redacted -}]]

inputInUse :: [[Int]]
inputInUse = actualInput

getAHalf :: (Int -> String -> String) -> Int -> String
getAHalf fn a = fn (len `div` 2) (show a)
    where 
        len = length (show a)

getFirstHalf :: Int -> String
getFirstHalf = getAHalf take
getSecondHalf :: Int -> String 
getSecondHalf = getAHalf drop

isEqualHalves :: Int -> Bool
isEqualHalves a = getFirstHalf a == getSecondHalf a 

filterEqualHalves :: [Int] -> [Int]  
filterEqualHalves = filter isEqualHalves

invalidIdsStarOne = map filterEqualHalves inputInUse

starOne = sum (map sum invalidIdsStarOne) -- I'm sure there is a cleaner way to do this (flat map / fold) || TODO: put the refactored version below in the file.

{-
 - DAY 2 | Puzzle 2 | Star 4/24
-}

getPartsOfLength :: String -> Int -> [String]
getPartsOfLength [] _ = []
getPartsOfLength idString len = take len idString : getPartsOfLength (drop len idString) len 

getNParts :: String -> Int -> [String]
getNParts idStr n = getPartsOfLength idStr (length idStr `div` n)

isAllEqualParts :: [String] -> Bool
isAllEqualParts parts = all (\x -> x == head parts) parts

areNPartsEqual :: String -> Int -> Bool
areNPartsEqual idStr n = isAllEqualParts (getNParts idStr n)

canDivideInNParts :: String -> Int -> Bool
canDivideInNParts _ 0 = False
canDivideInNParts _ 1 = False
canDivideInNParts idStr n = length idStr `rem` n == 0 

isInvalidIdStarTwoRecursive :: String -> Int -> Bool
isInvalidIdStarTwoRecursive _ 1 = False
isInvalidIdStarTwoRecursive idStr n = canDivideInNParts idStr n && areNPartsEqual idStr n || isInvalidIdStarTwoRecursive idStr (n-1)

isInvalidIdStarTwo :: Int -> Bool
isInvalidIdStarTwo a = isInvalidIdStarTwoRecursive (show a) (length (show a))

invalidIdsStarTwo = map (filter isInvalidIdStarTwo) inputInUse

starTwo = sum (map sum invalidIdsStarTwo) -- Same here, refactor
