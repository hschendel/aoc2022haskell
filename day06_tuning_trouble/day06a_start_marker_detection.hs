import Data.List

startMarker (a:b:c:d:l) =
    if (length (nub [a,b,c,d])) == 4 then 4
    else 1 + (startMarker (b:c:d:l))

main = do
    f <- readFile "input.txt"
    print (startMarker f)