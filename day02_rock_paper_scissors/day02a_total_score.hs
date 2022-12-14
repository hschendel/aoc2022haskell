import Data.List

shapeScore "A" = 1
shapeScore "B" = 2
shapeScore "C" = 3

mapOwnMove "X" = "A"
mapOwnMove "Y" = "B"
mapOwnMove "Z" = "C"

roundScore oppMove ownMove =
    if ownMove == oppMove then 3
    else if ownMove == "A" then
        if oppMove == "C" then 6 else 0
    else if ownMove == "B" then
        if oppMove == "A" then 6 else 0
    else
        if oppMove == "B" then 6 else 0

lineScore line =
    let [oppMove, unmappedOwnMove] = words line in
    let ownMove = mapOwnMove unmappedOwnMove in
    (shapeScore ownMove) + roundScore oppMove ownMove

main = do
    f <- readFile "input.txt"
    let sum = foldl' (+) 0 (map lineScore (lines f))
    print sum
