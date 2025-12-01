
{-
 - DAY 1 | Puzzle 1 | Star 1/24
-}

exampleInput = [{- Redacted -}]  

actualInput = [{- Redacted -}]

inputInUse = actualInput

dialStart :: Int
dialStart = 50 :: Int

dialNums :: [Int]
dialNums = [0..99] :: [Int]

dialSize :: Int
dialSize = length dialNums

dial :: [Int]
dial = cycle dialNums

turnDialIdx :: Int -> Int -> Int
turnDialIdx idx amt
    | amt < 0 = turnDialIdx idx ((amt `rem` dialSize) + 100) -- could use mod here instead
    | amt > 0 = idx + amt
    | otherwise = idx

mapToDialIdx :: [Int] -> Int -> [Int]
mapToDialIdx [] _ = []
mapToDialIdx (input: inputs) idx = newDial : mapToDialIdx inputs newDial
    where 
        newDial = turnDialIdx idx input

dialCheckpoints :: [Int]
dialCheckpoints = map (dial !!) (mapToDialIdx inputInUse dialStart)

starOne :: Int
starOne = length (filter (== 0) dialCheckpoints)

{-
 - DAY 1 | Puzzle 2 | Star 2/24
-}

amountOfRotations :: Int -> Int
amountOfRotations x = (abs x) `div` dialSize

zeroOrOne :: Num a => Int -> Int -> a
zeroOrOne start turnAmount 
    | turnAmount == 0 || start == 0 = 0
    | turnAmount > 0 = if start + turnAmount >= dialSize then 1 else 0
    | turnAmount < 0 = if start + turnAmount <= 0 then 1 else 0

mapToDialAllZeros :: [Int] -> [Int] -> Int
mapToDialAllZeros [] _ = 0
mapToDialAllZeros _ [] = 0
mapToDialAllZeros (input: inputs) (checkpoint: checkpoints) = ((amountOfRotations input) + (zeroOrOne checkpoint (input `rem` dialSize))) + mapToDialAllZeros inputs checkpoints

starTwo = mapToDialAllZeros inputInUse (dialStart:dialCheckpoints)
