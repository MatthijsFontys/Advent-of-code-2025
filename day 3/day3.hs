
{-
 - DAY 3 | Puzzle 1 | Star 5/24
-}

exampleInput :: [[Int]]
exampleInput = [{- Redacted -}]

actualInput :: [[Int]]
actualInput = [{- Redacted -}]

inputInUse :: [[Int]]
inputInUse = actualInput

getFirstJoltage :: [Int] -> Int
getFirstJoltage [] = 0
getFirstJoltage nums = maximum (init nums)

getSecondJoltage :: [Int] -> Int -> Int
getSecondJoltage [] _ = 0
getSecondJoltage nums firstJoltage = maximum (tail (dropWhile (firstJoltage >) nums))

getJoltageStarOne :: [Int] -> Int
getJoltageStarOne [] = 0
getJoltageStarOne nums = (firstJoltage * 10) + getSecondJoltage nums firstJoltage
  where 
    firstJoltage = getFirstJoltage nums

joltageStarOneArr :: [[Int]] -> [Int]
joltageStarOneArr [] = []
joltageStarOneArr nums2D = map getJoltageStarOne nums2D

starOne :: Int
starOne = sum (joltageStarOneArr inputInUse)

{-
 - DAY 3 | Puzzle 2 | Star 6/24
-}

takeHighestAllowed :: [Int] -> Int -> Int
takeHighestAllowed [] _ = 0
takeHighestAllowed nums amtToLeave = maximum (take amtToTake nums)
  where
    amtToTake = length nums - amtToLeave

getLeftovers :: [Int] -> Int -> [Int]
getLeftovers nums amtToLeave = tail (dropWhile (highest >) nums)
  where highest = takeHighestAllowed nums amtToLeave

batteriesForBank :: Int -> [Int] -> [Int]
batteriesForBank _ [] = []
batteriesForBank (-1) _ = []
batteriesForBank amtToLeave nums = highest : batteriesForBank (amtToLeave - 1) leftovers
  where 
    highest = takeHighestAllowed nums amtToLeave
    leftovers = getLeftovers nums amtToLeave

inputAsBankJoltage :: [[Int]]
inputAsBankJoltage = map (batteriesForBank 11) inputInUse

batteriesToSummable :: [Int] -> [Int]
batteriesToSummable [] = []
batteriesToSummable (x:xs) = (10 ^ length xs) * x : batteriesToSummable xs

summableBankJoltage :: [[Int]]
summableBankJoltage = map batteriesToSummable inputAsBankJoltage

joltagePerBank :: [Int]
joltagePerBank = map sum summableBankJoltage

starTwo = sum joltagePerBank
