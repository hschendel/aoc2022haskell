import Data.List

parse f = parseLines (lines f)
parseLines [] = []
parseLines (l:ls) = (parseLine l):(parseLines ls)

parseLine "" = []
parseLine (c:cl) = ((read (c:""))::Int):(parseLine cl)

visibleFromStart row = visibleFromStartR (-1) row
visibleFromStartR minHeight [] = []
visibleFromStartR minHeight (h:hl) = (h > minHeight):(visibleFromStartR (max minHeight h) hl)

orMap m1 m2 = zipWith (zipWith (||)) m1 m2

visibleMap :: [[Int]] -> [[Bool]]
visibleMap rows =
    let visibleFromLeft = map visibleFromStart rows
        visibleFromRight = map reverse (map visibleFromStart (map reverse rows))
        visibleFromLeftAndRight = visibleFromLeft `orMap` visibleFromRight
        cols = transpose rows
        visibleFromTop = map visibleFromStart cols
        visibleFromBottom = map reverse (map visibleFromStart (map reverse cols))
        visibleFromTopAndBottom = visibleFromTop `orMap` visibleFromBottom
    in  visibleFromLeftAndRight `orMap` (transpose visibleFromTopAndBottom)

countTruesInList [] = 0
countTruesInList (True:bl) = 1 + (countTruesInList bl)
countTruesInList (False:bl) = countTruesInList bl

countTruesInMap [] = 0
countTruesInMap (l:ls) = (countTruesInList l) + (countTruesInMap ls)

main = do
    f <- readFile "input.txt"
    let m = parse f
    let v = visibleMap m
    let numVisible = countTruesInMap v
    print numVisible