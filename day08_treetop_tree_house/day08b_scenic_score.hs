import Data.List

parse f = parseLines (lines f)
parseLines [] = []
parseLines (l:ls) = (parseLine l):(parseLines ls)

parseLine "" = []
parseLine (c:cl) = ((read (c:""))::Int):(parseLine cl)

viewingDistanceToEnd [] = []
viewingDistanceToEnd (h:hl) = (viewingDistanceFor h hl):(viewingDistanceToEnd hl)

viewingDistanceFor belowHeight [] = 0
viewingDistanceFor belowHeight (h:hl) =
    if h < belowHeight then 1 + (viewingDistanceFor belowHeight hl)
    else 1

multMap m1 m2 = zipWith (zipWith (*)) m1 m2

scoreMap scoringFunc addFunc rows =
    let fromLeft = map scoringFunc rows
        fromRight = map reverse (map scoringFunc (map reverse rows))
        fromLeftAndRight = addFunc fromLeft fromRight
        cols = transpose rows
        fromTop = map scoringFunc cols
        fromBottom = map reverse (map scoringFunc (map reverse cols))
        fromTopAndBottom = addFunc fromTop fromBottom
    in  addFunc fromLeftAndRight (transpose fromTopAndBottom)

maxFromMap m = foldl' max 0 (map (foldl' max 0) m)

main = do
    f <- readFile "input.txt"
    let m = parse f
    let scenicScores = scoreMap viewingDistanceToEnd multMap m
    let maxScenicScore = maxFromMap scenicScores
    print maxScenicScore