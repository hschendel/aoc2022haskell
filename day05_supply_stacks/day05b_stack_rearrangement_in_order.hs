import Data.List

parseStacksLine (' ':'1':_) = []
parseStacksLine "" = []
parseStacksLine ('[':x:']':l) = (Just x) : parseStacksLine (drop 1 l)
parseStacksLine (' ':' ':' ':l) = Nothing : parseStacksLine (drop 1 l)

appendStacksLine stacks [] = stacks
appendStacksLine [] (Nothing:entries) = [] : (appendStacksLine [] entries)
appendStacksLine (stack:stacks) (Nothing:entries) = stack : (appendStacksLine stacks entries)
appendStacksLine [] ((Just x):entries) = [x] : (appendStacksLine [] entries)
appendStacksLine (stack:stacks) ((Just x):entries) = (stack ++ [x]) : (appendStacksLine stacks entries)

parseStacks stacks (l:ls) =
    let entries = parseStacksLine l in
    if entries == [] then (stacks, (drop 1 ls))
    else parseStacks (appendStacksLine stacks entries) ls

isDigit c = '0' <= c && c <= '9'

takeIntPrefix s =
    let is = takeWhile isDigit s in
    ((read is)::Int, drop (length is) s)

parseMoveLine s =
    if (take 5 s) /= "move " then Nothing
    else
        let (count, s2) = takeIntPrefix (drop 5 s) in
        if (take 6 s2) /= " from " then Nothing
        else
            let (from, s3) = takeIntPrefix (drop 6 s2) in
            if (take 4 s3) /= " to " then Nothing
            else
                let to = (read (drop 4 s3))::Int in
                Just (count, from-1, to-1)

filterMoveLines [] = []
filterMoveLines (Nothing:_) = []
filterMoveLines ((Just move):ls) = move : (filterMoveLines ls)

parseMoveLines ls = filterMoveLines (map parseMoveLine ls)

applyMovePickUp 0 count (stack:stacks) = ([drop count stack] ++ stacks, take count stack)
applyMovePickUp from count (stack:stacks) =
    let (stacks2, pickedUp) = applyMovePickUp (from-1) count stacks in
    (stack:stacks2, pickedUp)

applyMovePutDown 0 movedElems (stack:stacks) = (movedElems ++ stack) : stacks
applyMovePutDown to movedElems (stack:stacks) = stack : (applyMovePutDown (to-1) movedElems stacks)

applyMove (count, from, to) stacks =
    let (stacks2, movedElems) = applyMovePickUp from count stacks in
    applyMovePutDown to movedElems stacks2

applyMoves stacks [] = stacks
applyMoves stacks (move:moves) = applyMoves (applyMove move stacks) moves

topOfStacks [] = []
topOfStacks ([]:stacks) = ' ' : (topOfStacks stacks)
topOfStacks ((top:stack):stacks) = top : (topOfStacks stacks)

calc f =
    let (initialStacks, ls) = parseStacks [] (lines f) in
    let finalStacks = applyMoves initialStacks (parseMoveLines ls) in
    topOfStacks finalStacks

main = do
    f <- readFile "input.txt"
    print (calc f)