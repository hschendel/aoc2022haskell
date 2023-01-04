import Data.Char (isDigit)
import qualified Data.Set as Set
import Data.List (foldl', sort)
import Data.Maybe (catMaybes)

dist (ax, ay) (bx, by) = (abs (ax-bx)) + (abs (ay-by))

isDigitOrMinus c = (isDigit c) || (c == '-')

parseSensor s =
    let s1 = drop 12 s
        (sxs, s2) = span isDigitOrMinus s1
        (sys, s3) = span isDigitOrMinus (drop 4 s2)
        (bxs, s4) = span isDigitOrMinus (drop 25 s3)
        bys = takeWhile isDigitOrMinus (drop 4 s4)
        sx = (read sxs)::Int
        sy = (read sys)::Int
        bx = (read bxs)::Int
        by = (read bys)::Int in
        ((sx, sy), dist (sx, sy) (bx, by), (bx, by))

sensorOverlapWithRow ry ((x,y), radius, _) =
    let dy = ry - y
        width = radius - (abs dy) in
        if width < 0 then Nothing
        else Just (x-width, x+width)

mergeSortedRanges :: [(Int,Int)] -> [(Int,Int)]
mergeSortedRanges [] = []
mergeSortedRanges [a] = [a]
mergeSortedRanges ((a1,a2):(b1,b2):rl) =
    if (a2 + 1) < b1 then (a1,a2):(mergeSortedRanges ((b1,b2):rl))
    else mergeSortedRanges ((a1, max a2 b2):rl)

mergeRanges rl = mergeSortedRanges (sort rl)

part1 ls ry =
    let sensors = map parseSensor ls
        knownBeaconsCount = Set.size (Set.fromList (filter (\(x,y) -> y == ry) (map (\(_, _, (bx,by)) -> (bx,by)) sensors)))
        excludedBySensors = mergeRanges (catMaybes (map (sensorOverlapWithRow ry) sensors))
        excludedCount = sum (map (\(a1,a2) -> a2-a1+1) excludedBySensors) in
        excludedCount - knownBeaconsCount

notInRanges [] start limit = Just start
notInRanges ((a1,a2):rs) start limit =
    if start < a1 then Just start
    else if limit <= a2 then Nothing
    else notInRanges rs (a2+1) limit

searchBeacon sensors n ry =
    if ry > n then (0,0)
    else
        let rowRanges = mergeRanges (catMaybes (map (sensorOverlapWithRow ry) sensors)) in
        case notInRanges rowRanges 1 n of
            Nothing -> searchBeacon sensors n (ry+1)
            Just x -> (x, ry)

part2 ls n =
    let sensors = map parseSensor ls
        (bx, by) = searchBeacon sensors n 1 in
    bx * 4000000 + by

main = do
    f <- readFile "input.txt"
    let ls = lines f
    print (part1 ls 2000000)
    print (part2 ls 4000000)