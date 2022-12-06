import Data.List

startMarker n l =
    if (length (nub (take n l))) == n then n
    else 1 + (startMarker n (tail l))

main = do
    f <- readFile "input.txt"
    print (startMarker 14 f)