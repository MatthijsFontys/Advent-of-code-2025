
exampleRanges :: [(Int, Int)]
exampleRanges = [(3, 5),(16, 20),(10, 14),(12, 18)]
exampleInput :: [Int]
exampleInput = [1,5,8,11,17,32]

useActual :: Bool
useActual = False

actualRanges :: [(Int, Int)]
actualRanges = [{- Redacted -}]
actualInput :: [Int]
actualInput = [{- Redacted -}]

ranges :: [(Int, Int)]
ranges = if useActual then actualRanges else exampleRanges
input :: [Int]
input = if useActual then actualInput else exampleInput


{-
 - DAY 5 | Puzzle 1 | Star 9/24
-}

isInRange :: Ord a => a -> (a, a) -> Bool
isInRange num (x, y) = num >= x && num <= y

isInAnyRange :: Int -> Bool
isInAnyRange num = any (isInRange num) ranges

starOne :: Int
starOne = length (filter isInAnyRange input)


{-
 - DAY 5 | Puzzle 2 | Star 10/24
-}

hasOverlapSingle :: (Int, Int) -> (Int, Int) -> Bool
hasOverlapSingle (x1, y1) (x2, y2) = x1 >= x2 && x1 <= y2

hasOverlap :: (Int, Int) -> (Int, Int) -> Bool
hasOverlap a b = hasOverlapSingle a b || hasOverlapSingle b a

unite :: (Int, Int) -> (Int, Int) -> (Int, Int)
unite (x1, y1) (x2, y2) = (min x1 x2, max y1 y2)

combineRanges :: [(Int, Int)] -> [(Int, Int)]
combineRanges [] = []
combineRanges (x:xs) = foldl unite x doOverlap : combineRanges noOverlap
    where 
        noOverlap = filter (not . hasOverlap x) xs
        doOverlap = filter (hasOverlap x) xs

combineRangesRec :: Int -> [(Int, Int)] -> [(Int, Int)]
combineRangesRec len tuples = if length combined == len then combined else combineRangesRec (length combined) combined
    where 
        combined = combineRanges tuples

combinedRanges :: [(Int, Int)]
combinedRanges = combineRangesRec (length ranges) ranges

starTwo :: Int
starTwo = foldl (\acc (x, y) -> y - x + 1 + acc) 0 combinedRanges