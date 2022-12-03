import Data.List
import Data.Char
import Control.Exception
import Control.Exception.Base

compartments s =
    let halfLen = length s `div` 2 in
        (take halfLen s, drop halfLen s)

sharedItem (a:al) bl =
    if elem a bl then a
    else sharedItem al bl

itemPriority c =
    if 'a' <= c && c <= 'z' then 1 + ord(c) - ord('a')
    else if 'A' <= c && c <= 'Z' then 27 + ord(c) - ord('A')
    else throw (AssertionFailed "invalid item char")

linePriority s =
    let (c1, c2) = compartments s in
    itemPriority (sharedItem c1 c2)

main = do
    f <- readFile "input.txt"
    print (foldl' (+) 0 (map linePriority (lines f)))