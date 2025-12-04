
{-
 - DAY 4 | Puzzle 1 | Star 7/24
-}

exampleInput :: String
exampleInput = "..@@.@@@@.@@@.@.@.@@@@@@@.@.@@@.@@@@..@.@@.@@@@.@@.@@@@@@@.@.@.@.@.@@@@.@@@.@@@@.@@@@@@@@.@.@.@@@.@."

exampleWidth :: Int
exampleWidth = length "..@@.@@@@."

exampleHeight :: Int
exampleHeight = length ".@@@@..@.@"

actualInput :: String
actualInput = ""

actualWidth :: Int
actualWidth = 0

actualHeight :: Int
actualHeight = 0

input :: String
input = exampleInput

width :: Int
width = exampleWidth

height :: Int
height = exampleHeight

-- zero based indexes --

posToIndex :: (Int, Int) -> Int
posToIndex (x, y) = x * width + y

indexToPos :: Int -> (Int, Int)
indexToPos i = (x, y)
  where
    x = i `div` width
    y = i - (x * width)

addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x1 ,y1) (x2, y2) = (x1 + x2, y1 + y2)

dirs :: [(Int, Int)]
dirs = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

isInBounds :: (Int, Int) -> (Int, Int) -> Bool
isInBounds posA posB = x < width && x >= 0 && y < height && y >= 0
  where
    total = addPos posA posB
    x = fst total
    y = snd total

posToNum :: (Int, Int) -> Int
posToNum pos = if c == '@' then 1 else 0
  where c = input !! posToIndex pos

dirsInBoundsFn :: String -> Int -> [[(Int, Int)]]
dirsInBoundsFn [] _ = []
dirsInBoundsFn ('.':xs) i = dirsInBoundsFn xs (i+1)
dirsInBoundsFn (x:xs) i = filter (isInBounds (indexToPos i)) dirs : dirsInBoundsFn xs (i+1)

dirsInBounds :: [[(Int, Int)]]
dirsInBounds = dirsInBoundsFn input 0


indexesInBoundsFn :: [[(Int, Int)]] -> Int -> [[Int]]
indexesInBoundsFn [] _  = []
indexesInBoundsFn (x:xs) i = map (posToIndex . addPos (indexToPos i)) x : indexesInBoundsFn xs (i+1)

indexesInBounds :: [[Int]]
indexesInBounds = indexesInBoundsFn dirsInBounds 0

starOne :: Int
starOne = 1

{-
 - DAY 4 | Puzzle 2 | Star 8/24
-}


starTwo = 1
