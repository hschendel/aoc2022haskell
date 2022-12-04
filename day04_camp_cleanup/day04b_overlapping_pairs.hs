parseLine s =
    let (n1, s2) = span (/= '-') s in
    let (n2, s3) = span (/= ',') (tail s2) in
    let (n3, s4) = span (/= '-') (tail s3) in
    let n4 = tail s4 in
    (((read n1)::Int, (read n2)::Int), ((read n3)::Int, (read n4)::Int))

overlaps ((a1,a2),(b1,b2)) = a1 <= b2 && a2 >= b1 || b1 <= a2 && b2 >= a1

main = do
    f <- readFile "input.txt"
    print (length (filter overlaps (map parseLine (lines f))))