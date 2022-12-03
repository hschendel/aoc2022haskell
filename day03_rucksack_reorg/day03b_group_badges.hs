import Data.List
import Data.Char
import Control.Exception
import Control.Exception.Base

itemPriority c =
    if 'a' <= c && c <= 'z' then 1 + ord(c) - ord('a')
    else if 'A' <= c && c <= 'Z' then 27 + ord(c) - ord('A')
    else throw (AssertionFailed "invalid item char")

splits n [] = []
splits n l =
    let (prefix, l2) = splitAt 3 l in
    prefix : (splits n l2)

groupItem g = head (foldl' intersect (head g) (tail g))

groupItemPriority g = itemPriority (groupItem g)

main = do
    f <- readFile "input.txt"
    print (foldl' (+) 0 (map groupItemPriority (splits 3 (lines f))))