import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isDigit)
import Data.List (foldl')

insertLine (fromX,fromY) (toX,toY) m =
    let row = case Map.lookup fromY m of
            Just r -> r
            Nothing -> Set.empty
        row2 = Set.insert fromX row
        m2 = Map.insert fromY row2 m in
    if fromX == toX && fromY == toY then m2
    else if fromX < toX then insertLine (fromX+1, fromY) (toX, toY) m2
    else if fromX > toX then insertLine (fromX-1, fromY) (toX, toY) m2
    else if fromY < toY then insertLine (fromX, fromY+1) (toX, toY) m2
    else insertLine (fromX, fromY-1) (toX, toY) m2

parsePoints "" = []
parsePoints s =
    let s2 = dropWhile (\c -> not (isDigit c)) s
        (sx, r1) = span isDigit s2
        x = (read sx)::Int
        (sy, r2) = span isDigit (drop 1 r1)
        y = (read sy)::Int in
    (x, y):(parsePoints r2)

insertPoints m [] = m
insertPoints m (_:[]) = m
insertPoints m (a:b:ps) =
    let m2 = insertLine a b m in
    insertPoints m2 (b:ps)

data DropResult = Fallthrough | Blocked | Rested deriving (Show, Eq)

dropSand :: Int -> Int -> Int -> Map.Map Int (Set.Set Int) -> (DropResult, Map.Map Int (Set.Set Int))
dropSand bottomY y x m =
    if (bottomY == (-1)) && (y > (fst (Map.findMax m))) then (Fallthrough, m)
    else if (bottomY > 0) && (y >= bottomY) then (Blocked, m)
    else
        let row = case Map.lookup y m of
                Just r -> r
                Nothing -> Set.empty in
            if Set.member x row then (Blocked, m)
            else
                case dropSand bottomY (y+1) x m of
                    (Blocked, _) -> case dropSand bottomY (y+1) (x-1) m of
                        (Blocked, _) -> case dropSand bottomY (y+1) (x+1) m of
                            (Blocked, _) ->
                                let row2 = Set.insert x row
                                    mr = Map.insert y row2 m in
                                (Rested, mr)
                            rightRes -> rightRes
                        leftRes -> leftRes
                    belowRes -> belowRes

dropUntilDone bottomY n m =
    case dropSand bottomY 0 500 m of
        (Blocked, _) -> (Blocked, n)
        (Fallthrough, _) -> (Fallthrough, n)
        (Rested, m2) -> dropUntilDone bottomY (n+1) m2

part1 m = dropUntilDone (-1) 0 m

part2 m =
    let (maxY, _) = Map.findMax m
        bottomY = maxY + 2 in
    dropUntilDone bottomY 0 m

main = do
    f <- readFile "input.txt"
    let m = foldl' (\m l -> insertPoints m (parsePoints l)) Map.empty (lines f)
    print (part1 m)
    print (part2 m)
