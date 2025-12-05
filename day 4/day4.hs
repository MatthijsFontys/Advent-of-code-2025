exampleInput :: String
exampleInput = "..@@.@@@@.@@@.@.@.@@@@@@@.@.@@@.@@@@..@.@@.@@@@.@@.@@@@@@@.@.@.@.@.@@@@.@@@.@@@@.@@@@@@@@.@.@.@@@.@."

exampleWidth :: Int
exampleWidth = 10

exampleHeight :: Int
exampleHeight = 10

useActual :: Bool
useActual = False

actualInput :: String
actualInput = "" -- redacted

actualWidth :: Int
actualWidth = 1 -- redacted

actualHeight :: Int
actualHeight = 1 -- redacted

input :: String
input = if useActual then actualInput else exampleInput

width :: Int
width = if useActual then actualWidth else exampleWidth

height :: Int
height = if useActual then actualHeight else exampleHeight


-- zero based indexed helpers --

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

posIsPaper :: (Int, Int) -> String -> Bool
posIsPaper pos room = room !! posToIndex pos == '@'

posToNum :: (Int, Int) -> String -> Int
posToNum pos room = if posIsPaper pos room then 1 else 0

inBoundsForPos :: (Int, Int) -> String -> [(Int, Int)]
inBoundsForPos pos room = filter (\x -> posIsPaper pos room && isInBounds x pos) dirs

allPos :: [(Int, Int)]
allPos = map indexToPos [0..(width*height - 1)]

-- END HELPERS --

{-
 - DAY 4 | Puzzle 1 | Star 7/24
-}

toCheck :: String -> [[(Int, Int)]]
toCheck room = map (\x -> map (addPos x) (inBoundsForPos x room)) allPos

checked :: String -> [[Int]]
checked room = (map.map) (flip posToNum room) (toCheck room)

getReachableIndexes :: [[Int]] -> Int -> [Int]
getReachableIndexes [] _ = []
getReachableIndexes (x:xs) i =  if sum x < 4 && length x > 0 
  then i : getReachableIndexes xs (i + 1)
  else getReachableIndexes xs (i + 1)

reachableIndexes :: String -> [Int]
reachableIndexes room = (getReachableIndexes (checked room) 0)

reachableAmt :: String -> Int
reachableAmt room = length $ reachableIndexes room

starOne :: Int
starOne = reachableAmt input

{-
 - DAY 4 | Puzzle 2 | Star 8/24
-}

updateRoom :: [Int] -> String -> String
updateRoom [] room = room
updateRoom (x:xs) room = updateRoom xs (roomPrefix ++ "." ++ roomSuffix)
  where 
    roomPrefix = take x room
    roomSuffix = drop (x+1) room

cleanedPaperAmtFn :: String -> Int -> Int
cleanedPaperAmtFn room total = if amount == 0 then total else cleanedPaperAmtFn (updateRoom indexes room) (total + amount)
  where 
    indexes = reachableIndexes room
    amount = length indexes

starTwo :: Int
starTwo = cleanedPaperAmtFn input 0
