sums g 0 [] = g
sums g n [] = g ++ [n]
sums g n (s:ss) =
    if s == "" then sums (g ++ [n]) 0 ss
    else sums g (n + (read s)) ss

main = do
    f <- readFile "input.txt"
    let ls = lines f
    let elfSums = sums [] 0 ls
    let maxCalories = maximum elfSums
    print maxCalories
