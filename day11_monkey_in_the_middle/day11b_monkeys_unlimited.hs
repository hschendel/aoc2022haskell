import Data.List

data Monkey = Monkey { items :: [Int]
    , operation :: (Int -> Int)
    , divisibleBy :: Int
    , ifTrueMonkey :: Int
    , ifFalseMonkey :: Int
    , inspectCount :: Int }

nextMonkey m worryLevel =
    if (worryLevel `mod` (divisibleBy m)) == 0 then ifTrueMonkey m
    else ifFalseMonkey m

addToInspectCount n m = Monkey { items = items m, operation = operation m, divisibleBy = divisibleBy m,
                        ifTrueMonkey = ifTrueMonkey m, ifFalseMonkey = ifFalseMonkey m,
                        inspectCount = (inspectCount m) + n}

appendToItems is m = Monkey { items = (items m) ++ is, operation = operation m, divisibleBy = divisibleBy m,
                       ifTrueMonkey = ifTrueMonkey m, ifFalseMonkey = ifFalseMonkey m,
                       inspectCount = inspectCount m}

clearItems m = Monkey { items = [], operation = operation m, divisibleBy = divisibleBy m,
                      ifTrueMonkey = ifTrueMonkey m, ifFalseMonkey = ifFalseMonkey m,
                      inspectCount = inspectCount m}

parseMonkeys [] = []
parseMonkeys ("":ls) = parseMonkeys ls
parseMonkeys (_:startingItemsLine:operationLine:testLine:ifTrueLine:ifFalseLine:ls) =
    (Monkey {
        items = read ("[" ++  (drop 18 startingItemsLine) ++ "]"),
        operation = parseOp (drop 23 operationLine),
        divisibleBy = read (drop 21 testLine),
        ifTrueMonkey = read (drop 29 ifTrueLine),
        ifFalseMonkey = read (drop 30 ifFalseLine),
        inspectCount = 0
        }):(parseMonkeys ls)

parseOp s =
    let (operator, operand2) = span (/= ' ') s
        f = case operator of
            "+" -> (+)
            "*" -> (*)
            _ -> error ("unknown operator: " ++ operator ++ " in operation " ++ s) in
   if operand2 == " old" then (\x -> f x x)
   else let v2 = (read operand2)::Int in (\x -> f x v2)

turnResults p m [] = []
turnResults p m (worryLevel1:items) =
    let worryLevel2 = (operation m) worryLevel1
        worryLevel3 = worryLevel2 `mod` p
        targetMonkey = nextMonkey m worryLevel3 in
    (targetMonkey, worryLevel3):(turnResults p m items)

turn p monkeys i =
    let m = monkeys !! i
        toAppend = turnResults p m (items m)
        m2 = addToInspectCount (length (items m)) (clearItems m)
        monkeys2 = (take (i) monkeys) ++ [m2] ++ (drop (i+1) monkeys) in
    appendItems toAppend monkeys2

appendItems [] monkeys = monkeys
appendItems ((i, item):is) monkeys =
    let (ms1, (m:ms2)) = splitAt i monkeys
        monkeys2 = ms1 ++ [appendToItems [item] m] ++ ms2 in
    appendItems is monkeys2

monkeyRound p monkeys =
    let roundR p i monkeys = if i >= (length monkeys)
        then monkeys
        else roundR p (i+1) (turn p monkeys i) in
    roundR p 0 monkeys

monkeyRounds p 0 monkeys = monkeys
monkeyRounds p n monkeys = monkeyRounds p (n-1) (monkeyRound p monkeys)

monkeyBusinessLevel monkeys =
    let inspectCounts = map inspectCount monkeys
        (top1:top2:_) = sortBy (flip compare) inspectCounts in
        top1 * top2

part2 monkeys =
    let p = product (map divisibleBy monkeys) in
    monkeyBusinessLevel (monkeyRounds p 10000 monkeys)

main = do
    f <- readFile "input.txt"
    let monkeys = parseMonkeys (lines f)
    print (part2 monkeys)