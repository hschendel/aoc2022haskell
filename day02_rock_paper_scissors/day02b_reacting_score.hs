shapeScore "A" = 1
shapeScore "B" = 2
shapeScore "C" = 3

mapOwnMove "X" = "A"
mapOwnMove "Y" = "B"
mapOwnMove "Z" = "C"

loose "A" = "C"
loose "B" = "A"
loose "C" = "B"
win "A" = "B"
win "B" = "C"
win "C" = "A"

calcMove oppMove "X" = loose oppMove
calcMove oppMove "Y" = oppMove
calcMove oppMove "Z" = win oppMove

strategyScore "X" = 0
strategyScore "Y" = 3
strategyScore "Z" = 6

lineScore line =
    let [oppMove, strategy] = words line in
    let ownMove = (calcMove oppMove strategy) in
    (shapeScore ownMove) + (strategyScore strategy)

foldl2 f z []     = z
foldl2 f z (x:xs) = let z2 = z `f` x in seq z2 $ foldl2 f z2 xs

main = do
    f <- readFile "input.txt"
    let sum = foldl2 (+) 0 (map lineScore (lines f))
    print sum
