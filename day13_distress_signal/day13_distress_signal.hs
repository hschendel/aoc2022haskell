import Data.List (findIndices, findIndex, sort, intercalate)
import Data.Char (isDigit)
import Data.Maybe (fromJust)

data Entry = IntEntry Int | ListEntry [Entry]

instance Show Entry where
    show (IntEntry i) = show i
    show (ListEntry l) = "[" ++ (intercalate "," (map show l)) ++ "]"

parsePairs :: [String] -> [(Entry,Entry)]
parsePairs [] = []
parsePairs ("":l) = parsePairs l
parsePairs (a:b:l) =
    (parseEntry a, parseEntry b):(parsePairs l)

parseEntry :: String -> Entry
parseEntry s =
    let (e, r) = parseEntryR s in
    if r /= "" then error ("dangling input: " ++ (show r) ++ " from: " ++ (show s))
    else e

parseEntryR :: String -> (Entry, String)
parseEntryR ('[':s) = parseListEntry s
parseEntryR (c:s) =
    let (n, s2) = span isDigit (c:s)
        i = (read n)::Int in
        (IntEntry i, s2)

parseListEntry :: String -> (Entry, String)
parseListEntry "" = error "premature end of list"
parseListEntry (']':s) = (ListEntry [], s)
parseListEntry (',':s) = parseListEntry s
parseListEntry s =
    let (e, s2) = parseEntryR s
        (ListEntry lr, s3) = parseListEntry s2 in
    (ListEntry (e:lr), s3)

compareEntry :: Entry -> Entry -> Ordering
compareEntry (ListEntry []) (ListEntry []) = EQ
compareEntry (ListEntry []) _ = LT
compareEntry _ (ListEntry []) = GT
compareEntry (IntEntry a) (IntEntry b) = a `compare` b
compareEntry (ListEntry al) (IntEntry b) = compareEntry (ListEntry al) (ListEntry [IntEntry b])
compareEntry (IntEntry a) (ListEntry bl) = compareEntry (ListEntry [IntEntry a]) (ListEntry bl)
compareEntry (ListEntry (a:al)) (ListEntry (b:bl)) =
    case compareEntry a b of
        LT -> LT
        GT -> GT
        EQ -> compareEntry (ListEntry al) (ListEntry bl)

instance Eq Entry where
    a == b = (compareEntry a b) == EQ

instance Ord Entry where
    compare a b = compareEntry a b

pairInOrder (a,b) = case compareEntry a b of
    GT -> False
    _ -> True

pairsToList [] = []
pairsToList ((a,b):l) = a:b:(pairsToList l)

part1 pairs =
    sum (map succ (findIndices pairInOrder pairs))

part2 pairs =
    let div1 = parseEntry "[[2]]"
        div2 = parseEntry "[[6]]"
        l = div1:div2:(pairsToList pairs)
        sorted = sort l
        div1is = 1 + (fromJust (findIndex (== div1) sorted))
        div2is = 1 + (fromJust (findIndex (== div2) sorted)) in
        div1is * div2is

main = do
    f <- readFile "input.txt"
    let pairs = parsePairs (lines f)
    print (part1 pairs)
    print (part2 pairs)