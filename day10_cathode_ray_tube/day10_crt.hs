import Data.List

execute _ [] = []
execute x ("noop":ls) = x:(execute x ls)
execute x (('a':'d':'d':'x':' ':s):ls) =
    let v = (read s)::Int in
    x:x:(execute (x+v) ls)

positionTuples i [] _ = []
positionTuples i _ [] = []
positionTuples i (p:pl) (v:vs) =
    if i == p then (p, v):(positionTuples (i+1) pl vs)
    else positionTuples (i+1) (p:pl) (vs)

signalStrength (pos, v) = pos * v

signalStrengthSum cycles vs =
    let posVals = positionTuples 1 cycles vs
        signalStrengths = map signalStrength posVals in
        sum signalStrengths

-- pixel positions start at 0
pixels i [] = []
pixels i (v:vs) =
    let lineX = i `mod` 40 in
    (if (v-1) <= lineX && lineX <= (v+1) then '#' else '.'):(pixels (i+1) vs)

pixelLines [] = []
pixelLines ps = (take 40 ps):(pixelLines (drop 40 ps))

main = do
    f <- readFile "input.txt"
    let vs = (execute 1 (lines f))
    let result1 = signalStrengthSum [20, 60, 100, 140, 180, 220] vs
    print result1
    let screen = unlines (take 6 (pixelLines (pixels 0 vs)))
    putStr screen