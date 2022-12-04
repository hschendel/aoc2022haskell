parseLine s =
    let (n1, s2) = span (/= '-') s in
    let (n2, s3) = span (/= ',') (tail s2) in
    let (n3, s4) = span (/= '-') (tail s3) in
    let n4 = tail s4 in
    (((read n1)::Int, (read n2)::Int), ((read n3)::Int, (read n4)::Int))

rangeIn (a1,a2) (b1,b2) = b1 <= a1 && a2 <= b2

isFullyContainingPair (a,b) = a `rangeIn` b || b `rangeIn` a

main = do
    f <- readFile "input.txt"
    print (length (filter isFullyContainingPair (map parseLine (lines f))))