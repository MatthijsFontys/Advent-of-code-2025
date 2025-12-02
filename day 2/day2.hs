
{-
 - DAY 1 | Puzzle 1 | Star 1/24
-}

exampleInput :: [[Int]]
exampleInput = [[11..22], [95..115], [998..1012], [1188511880..1188511890], [222220..222224], [1698522..1698528], [446443..446449], [38593856..38593862], [565653..565659], [824824821..824824827], [2121212118..2121212124]]

actualInput :: [[Int]]
actualInput = [[990244..1009337], [5518069..5608946], [34273134..34397466], [3636295061..3636388848], [8613701..8663602], [573252..688417], [472288..533253], [960590..988421], [7373678538..7373794411], [178..266], [63577667..63679502], [70..132], [487..1146], [666631751..666711926], [5896..10827], [30288..52204], [21847924..21889141], [69684057..69706531], [97142181..97271487], [538561..555085], [286637..467444], [93452333..93519874], [69247..119122], [8955190262..8955353747], [883317..948391], [8282803943..8282844514], [214125..236989], [2518..4693], [586540593..586645823], [137643..211684], [33..47], [16210..28409], [748488..837584], [1381..2281], [1..19]]

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
