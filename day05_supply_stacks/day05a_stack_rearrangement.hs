import Data.List

parseStacksLine [] (' ':'1':_) = []
parseStacksLine a "" = a
parseStacksLine a ('[':x:']':l) = parseStacksLine (a ++ [Just x]) (drop 1 l)
parseStacksLine a (' ':' ':' ':l) = parseStacksLine (a ++ [Nothing]) (drop 1 l)

appendStacksLine doneStacks stacks [] = doneStacks ++ stacks
appendStacksLine doneStacks [] (Nothing:entries) = appendStacksLine (doneStacks ++ [[]]) [] entries
appendStacksLine doneStacks (stack:stacks) (Nothing:entries) = appendStacksLine (doneStacks ++ [stack]) stacks entries
appendStacksLine doneStacks [] ((Just x):entries) = appendStacksLine (doneStacks ++ [[x]]) [] entries
appendStacksLine doneStacks (stack:stacks) ((Just x):entries) = appendStacksLine (doneStacks ++ [stack ++ [x]]) stacks entries

parseStacks stacks (l:ls) =
    let entries = parseStacksLine [] l in
    if entries == [] then (stacks, (drop 1 ls))
    else parseStacks (appendStacksLine [] stacks entries) ls

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

applyMovePickUp 0 count doneStacks (stack:stacks) = (doneStacks ++ [drop count stack] ++ stacks, take count stack)
applyMovePickUp from count doneStacks (stack:stacks) =
    applyMovePickUp (from-1) count (doneStacks ++ [stack]) stacks

applyMovePutDown 0 movedElems doneStacks (stack:stacks) = (doneStacks ++ [(reverse movedElems) ++ stack] ++ stacks)
applyMovePutDown to movedElems doneStacks (stack:stacks) =
    applyMovePutDown (to-1) movedElems (doneStacks ++ [stack]) stacks

applyMove (count, from, to) stacks =
    let (stacks2, movedElems) = applyMovePickUp from count [] stacks in
    applyMovePutDown to movedElems [] stacks2

applyMoves stacks [] = stacks
applyMoves stacks (move:moves) = applyMoves (applyMove move stacks) moves

topOfStacks seen [] = seen
topOfStacks seen ([]:stacks) = topOfStacks (seen ++ " ") stacks
topOfStacks seen ((top:stack):stacks) = topOfStacks (seen ++ [top]) stacks

calc f =
    let (initialStacks, ls) = parseStacks [] (lines f) in
    let finalStacks = applyMoves initialStacks (parseMoveLines ls) in
    topOfStacks "" finalStacks

main = do
    f <- readFile "input.txt"
    print (calc f)