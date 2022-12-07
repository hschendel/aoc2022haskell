import Data.List

data FsEntry = File Int | Dir [(String,FsEntry)] deriving (Eq, Show)
data LogLine = CommandLs [(String, Int)] | CommandCdDown String | CommandCdUp | CommandCdRoot deriving (Eq, Show)

parseInput [] = []
parseInput ("$ ls":ls) =
    let (es, ls2) = parseLsEntries ls in
    (CommandLs es):(parseInput ls2)
parseInput (('$':' ':'c':'d':' ':s):ls) = (
    if s == "/" then CommandCdRoot
    else if s == ".." then CommandCdUp
    else (CommandCdDown s)) : (parseInput ls)

parseLsEntries [] = ([], [])
parseLsEntries (('$':s):ls) = ([], ('$':s):ls)
parseLsEntries (('d':'i':'r':' ':s):ls) =
    -- skip directories, they are discovered through cd
    parseLsEntries ls
parseLsEntries (l:ls) =
    let (sizeS, nameS) = span (/= ' ') l in
    let size = (read sizeS)::Int in
    let name = drop 1 nameS in
    let (es, ls2) = parseLsEntries ls in
    ((name, size):es, ls2)

putFs (File _) (Dir es) = Dir es
putFs (Dir _) (File s) = File s
putFs (Dir es1) (Dir es2) =
    let es1s = sortOn fst es1 in
    let es2s = sortOn fst es2 in
    Dir (mergeSortedEntries es1s es2s)

putFsFile fs path size = putFs fs (pathToFs path size)

pathToFs [] size = File size
pathToFs (dirName:pathRem) size = Dir [(dirName, pathToFs pathRem size)]

mergeSortedEntries [] [] = []
mergeSortedEntries x [] = x
mergeSortedEntries [] y = y
mergeSortedEntries ((x,xe):xl) ((y,ye):yl) =
    if x < y then (x,xe):(mergeSortedEntries xl ((y,ye):yl))
    else if y < x then (y,ye):(mergeSortedEntries ((x,xe):xl) yl)
    else (x,(putFs xe ye)):(mergeSortedEntries xl yl)

dropLast [] = []
dropLast (_:[]) = []
dropLast (x:xl) = x:(dropLast xl)

processLogLines fs wd [] = (fs, wd)
processLogLines fs wd (l:ls) =
    let (fs2, wd2) = processLogLine fs wd l in
    processLogLines fs2 wd2 ls

processLogLine fs _ CommandCdRoot = (fs, [])
processLogLine fs wd CommandCdUp = (fs, (dropLast wd))
processLogLine fs wd (CommandCdDown dirName) = (fs, wd ++ [dirName])
processLogLine fs wd (CommandLs es) = (processLsEntries fs wd es, wd)

processLsEntries fs wd [] = fs
processLsEntries fs wd ((name,size):es) = processLsEntries (putFsFile fs (wd ++ [name]) size) wd es

filteredMaxTotalSizeSum maxSize (File size) = (0, size)
filteredMaxTotalSizeSum maxSize (Dir es) =
    let (filteredSum, total) = filteredMaxTotalSizeSumForEntries maxSize es in
    if total <= maxSize then (filteredSum + total, total)
    else (filteredSum, total)

filteredMaxTotalSizeSumForEntries maxSize [] = (0, 0)
filteredMaxTotalSizeSumForEntries maxSize ((name, entry):es) =
    let (entrySum, entryTotal) = filteredMaxTotalSizeSum maxSize entry in
    let (esSum, esTotal) = filteredMaxTotalSizeSumForEntries maxSize es in
    (entrySum + esSum, entryTotal + esTotal)

main = do
    f <- readFile "input.txt"
    let terminalLog = parseInput (lines f)
    let (fs, _) = processLogLines (Dir []) [] terminalLog
    let (filteredSum, _) = filteredMaxTotalSizeSum 100000 fs
    print filteredSum