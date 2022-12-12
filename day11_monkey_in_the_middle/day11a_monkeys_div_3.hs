import Data.List

data Monkey = Monkey { items :: [Int]
    , operation :: (Int -> Int)
    , nextMonkey :: (Int -> Int)
    , inspectCount :: Int }

parseMonkeys [] = []
parseMonkeys ("":ls) = parseMonkeys ls
parseMonkeys (_:startingItemsLine:operationLine:testLine:ifTrueLine:ifFalseLine:ls) =
    let items = read("[" ++  (drop 18 startingItemsLine) ++ "]")::[Int]
        operation = parseOp (drop 23 operationLine)
        divisibleBy = (read (drop 21 testLine))::Int
        ifTrueMonkey = (read (drop 29 ifTrueLine))::Int
        ifFalseMonkey = (read (drop 30 ifFalseLine))::Int
        nextMonkey = (\x -> if (x `mod` divisibleBy) == 0 then ifTrueMonkey else ifFalseMonkey) in
    (Monkey {items = items, operation = operation, nextMonkey = nextMonkey, inspectCount = 0}):(parseMonkeys ls)

parseOp s =
    let (operator, operand2) = span (/= ' ') s
        f = case operator of
            "+" -> (+)
            "*" -> (*)
            _ -> error ("unknown operator: " ++ operator ++ " in operation " ++ s) in
   if operand2 == " old" then (\x -> f x x)
   else let v2 = (read operand2)::Int in (\x -> f x v2)

turnResults op nextMonkey [] = []
turnResults op nextMonkey (worryLevel1:items) =
    let worryLevel2 = op worryLevel1
        worryLevel3 = worryLevel2 `div` 3
        targetMonkey = nextMonkey worryLevel3 in
    (targetMonkey, worryLevel3):(turnResults op nextMonkey items)

turn monkeys i =
    let m = monkeys !! i
        toAppend = turnResults (operation m) (nextMonkey m) (items m)
        m2 = Monkey {items = [], operation = (operation m), nextMonkey = (nextMonkey m), inspectCount = ((inspectCount m) + (length (items m)))}
        monkeys2 = (take (i) monkeys) ++ [m2] ++ (drop (i+1) monkeys) in
    appendItems toAppend monkeys2

appendItems [] monkeys = monkeys
appendItems ((i, item):is) monkeys =
    let (ms1, (m:ms2)) = splitAt i monkeys
        monkeys2 = ms1 ++ [Monkey {
                items = (items m) ++ [item],
                        operation = operation m,
                        nextMonkey = nextMonkey m,
                        inspectCount = inspectCount m
            }] ++ ms2 in
    appendItems is monkeys2

monkeyRound monkeys =
    let roundR i monkeys = if i >= (length monkeys)
        then monkeys
        else roundR (i+1) (turn monkeys i) in
    roundR 0 monkeys

monkeyRounds 0 monkeys = monkeys
monkeyRounds n monkeys = monkeyRounds (n-1) (monkeyRound monkeys)

monkeyBusinessLevel monkeys =
    let inspectCounts = map inspectCount monkeys
        (top1:top2:_) = sortBy (flip compare) inspectCounts in
        top1 * top2

part1 monkeys =
    monkeyBusinessLevel (monkeyRounds 20 monkeys)

main = do
    f <- readFile "input.txt"
    let monkeys = parseMonkeys (lines f)
    print (part1 monkeys)