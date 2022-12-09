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

step dir ((hx,hy), (tx,ty)) =
    let (hx2,hy2) = moveHead dir (hx,hy)
        lostContact = isContactLost (hx2,hy2) (tx,ty) in
    ((hx2,hy2), ((moveTailAxis lostContact hx2 tx), (moveTailAxis lostContact hy2 ty)))

process state [] = state
process state ((dir,0):ml) = process state ml
process (ropeState, visitedByTail) ((dir,n):ml) =
    let (h2,t2) = step dir ropeState in
    process ((h2,t2), Set.insert t2 visitedByTail) ((dir,(n-1)):ml)

calculateNumVisitedByTail ls =
    let (_, visitedByTail) = process (((0,0),(0,0)), (Set.fromList [(0,0)])) (parseLines ls) in
    Set.size visitedByTail

main = do
    f <- readFile "input.txt"
    let ls = lines f
    print (calculateNumVisitedByTail ls)