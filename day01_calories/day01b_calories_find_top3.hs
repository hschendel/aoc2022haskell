import Data.List

sums g 0 [] = g
sums g n [] = g ++ [n]
sums g n (s:ss) =
    if s == "" then sums (g ++ [n]) 0 ss
    else sums g (n + (read s)) ss

topThree t3 [] = t3
topThree t3 (n:ns) =
    if length t3 < 3 then topThree (sort (t3 ++ [n])) ns
    else
        let [a, b, c] = t3 in
        if a < n then topThree (sort [n, b, c]) ns
        else topThree t3 ns

main = do
    f <- readFile "input.txt"
    let ls = lines f
    let elfSums = sums [] 0 ls
    let t3 = topThree [] elfSums
    let t3sum = sum t3
    print t3sum
