import qualified Data.Set as Set

parseLines [] = []
parseLines ((dir:' ':n):ls) = (dir, (read n)::Int):(parseLines ls)

moveHead 'U' (x,y) = (x, (y+1))
moveHead 'D' (x,y) = (x, (y-1))
moveHead 'R' (x,y) = ((x+1), y)
moveHead 'L' (x,y) = ((x-1), y)

isContactLost (hx,hy) (tx,ty) =
    (abs (hx - tx)) > 1 || (abs (hy - ty)) > 1

moveTailAxis lostContact hPos tPos =
    let dist = hPos - tPos
        absDist = abs dist in
    if (absDist > 1 || lostContact && absDist > 0) then tPos + (signum dist)
    else tPos

step dir rope =
    let (hx2,hy2) = moveHead dir (head rope) in
    moveRope (hx2,hy2) (tail rope)

moveRope (lx,ly) [] = ([(lx,ly)], (lx,ly))
moveRope (lx,ly) ((fx,fy):rope) =
    let lostContact = isContactLost (lx,ly) (fx,fy)
        fx2 = moveTailAxis lostContact lx fx
        fy2 = moveTailAxis lostContact ly fy
        (rope2, tailPos2) = moveRope (fx2,fy2) rope in
        ((lx,ly):rope2, tailPos2)

process state [] = state
process state ((dir,0):ml) = process state ml
process (rope, visitedByTail) ((dir,n):ml) =
    let (rope2, tailPos2) = step dir rope in
    process (rope2, Set.insert tailPos2 visitedByTail) ((dir,(n-1)):ml)

calculateNumVisitedByTail ls =
    let (_, visitedByTail) = process (take 10 (repeat (0,0)), (Set.fromList [(0,0)])) (parseLines ls) in
    Set.size visitedByTail

main = do
    f <- readFile "input.txt"
    let ls = lines f
    print (calculateNumVisitedByTail ls)