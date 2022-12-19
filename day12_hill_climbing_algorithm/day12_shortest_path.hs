import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Maybe (catMaybes)

linesToMap ls =
    edgesToMap Map.empty (linesToEdges 1 ls)

edgesToMap m [] = m
edgesToMap m ((a,b):edges) =
    let aNeighbours = case Map.lookup a m of Nothing -> [b]; Just nl -> b:nl
        m2 = Map.insert a aNeighbours m in
        edgesToMap m2 edges

linesToEdges _ [] = []
linesToEdges y (l:[]) = lineToXEdges y 1 l
linesToEdges y (l1:l2:ls) =
    (lineToXEdges y 1 l1) ++ (lineToYEdges y 1 l1 l2) ++ (linesToEdges (y+1) (l2:ls))

lineToXEdges _ _ [] = []
lineToXEdges _ _ (_:[]) = []
lineToXEdges y x (c1:c2:cl) =
    let r1 = lineToXEdges y (x+1) (c2:cl)
        r2 = if (heightDiff c2 c1) <= 1 then ((x,y), (x+1,y)):r1 else r1 in
        if (heightDiff c1 c2) <= 1 then ((x+1,y), (x,y)):r2 else r2

lineToYEdges _ _ [] [] = []
lineToYEdges y x (a:al) (b:bl) =
    let r1 = lineToYEdges y (x+1) al bl
        r2 = if (heightDiff b a) <= 1 then ((x,y), (x,y+1)):r1 else r1 in
        if (heightDiff a b) <= 1 then ((x,y+1), (x,y)):r2 else r2

heightDiff c1 c2 =
    (ord (mapStartEnd c1)) - (ord (mapStartEnd c2))

shortestPossible (ax,ay) (bx,by) = (abs (ax - bx)) + (abs (ay - by))

mapStartEnd 'S' = 'a'
mapStartEnd 'E' = 'z'
mapStartEnd c = c

findStartEndAndAs sea _ [] = sea
findStartEndAndAs sea y (l:ls) =
    let sea2 = findStartEndAndAsInLine sea y 1 l in
    findStartEndAndAs sea2 (y+1) ls

findStartEndAndAsInLine se _ _ [] = se
findStartEndAndAsInLine (start, end, al) y x (c:cl) =
    let sea2 = case c of
              'S' -> ((x,y), end, (x,y):al)
              'E' -> (start, (x, y), al)
              'a' -> (start, end, (x,y):al)
              _ -> (start, end, al) in
    findStartEndAndAsInLine sea2 y (x+1) cl

neighbours v m =
    case Map.lookup v m of
    Nothing -> []
    Just nl -> nl

pqEmpty :: (Ord p, Ord v) => (Map.Map v p, Map.Map p [v])
pqEmpty = (Map.empty, Map.empty)

pqPut :: (Ord p, Ord v) => p -> v -> (Map.Map v p, Map.Map p [v]) -> (Map.Map v p, Map.Map p [v])
pqPut prio v (v2prio, prio2vs) =
    case Map.lookup v v2prio of
        Nothing -> (Map.insert v prio v2prio, appendToPrio2Vs prio v prio2vs)
        Just oldPrio -> (Map.insert v prio v2prio, appendToPrio2Vs prio v (removeFromPrio2Vs prio v prio2vs))

appendToPrio2Vs prio v prio2vs =
    case Map.lookup prio prio2vs of
        Nothing -> Map.insert prio [v] prio2vs
        Just vs -> Map.insert prio (v:vs) prio2vs

removeFromPrio2Vs prio v prio2vs =
    case Map.lookup prio prio2vs of
            Nothing -> prio2vs
            Just vs -> case filter (/= v) vs of
                [] -> Map.delete prio prio2vs
                vs2 -> Map.insert prio vs2 prio2vs

pqPop :: (Ord p, Ord v) => (Map.Map v p, Map.Map p [v]) -> (v, (Map.Map v p, Map.Map p [v]))
pqPop (v2prio, prio2vs) =
    let (prio, (v:vl)) = Map.findMin prio2vs
        prio2vs2 = removeFromPrio2Vs prio v prio2vs
        v2prio2 = Map.delete v v2prio in
    (v, (v2prio2, prio2vs2))

reconstructPath :: (Ord v) => Map.Map v v -> v -> [v]
reconstructPath cameFrom current =
    case Map.lookup current cameFrom of
        Nothing -> []
        Just current2 -> (reconstructPath cameFrom current2) ++ [current]

gScoreLookup v gScore = case Map.lookup v gScore of
    Nothing -> 999_999_999_999
    Just sc -> sc

aStar :: Ord v => (v -> Int) -> Map.Map v [v] -> v -> v -> Maybe [v]
aStar h neighbourMap start end =
    let gScore = Map.fromList [(start, 0)]
        openSet = pqPut (h start) start pqEmpty
        cameFrom = Map.empty in
    aStarRec h neighbourMap end (gScore, openSet, cameFrom)

aStarRec :: Ord v => (v -> Int) -> Map.Map v [v] -> v -> (Map.Map v Int, (Map.Map v Int, Map.Map Int [v]), Map.Map v v) -> Maybe [v]
aStarRec h neighbourMap end (gScore, openSet, cameFrom) =
    if openSet == pqEmpty then Nothing
    else
        let (current, openSet2) = pqPop openSet in
        if current == end then Just (reconstructPath cameFrom current)
        else aStarRec h neighbourMap end (foldl' (processNeighbor h current) (gScore, openSet2, cameFrom) (neighbours current neighbourMap))

processNeighbor h current (gScore, openSet, cameFrom) neighbour =
    let tentativeGScore = (gScoreLookup current gScore) + 1 in
    if tentativeGScore < (gScoreLookup neighbour gScore) then
        let newFScore = tentativeGScore + (h neighbour) in
        (Map.insert neighbour tentativeGScore gScore, pqPut newFScore neighbour openSet, Map.insert neighbour current cameFrom)
    else (gScore, openSet, cameFrom)

parse f =
    let ls = lines f
        neighbourMap = linesToMap ls
        (start, end, al) = findStartEndAndAs ((0,0), (0,0), []) 1 ls in
        (neighbourMap, start, end, al)

part1 (neighbourMap, start, end, _) =
    case aStar (shortestPossible end) neighbourMap start end of
        Nothing -> (-1, [])
        Just path -> (length path, path)

part2 (neighbourMap, _, end, al) =
    let paths = catMaybes (map (\start -> aStar (shortestPossible end) neighbourMap start end) al) in
    minimum (map length paths)

main = do
    f <- readFile "input.txt"
    let m = parse f
    print (part1 m)
    print (part2 m)
