# Advent of Code 2022 in Haskell

## Day 4 - Camp Cleanup

It spent some time studying the [Prelude documentation](https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.17.0.0/Prelude.html)
and other sources for an easy way to parse a line into four integers. In the past, there seems to have been
a library called `Text.Regex` but my version 9.2.5 of the "glorious" Glasgow Haskell Compiler did not have it.
As I did not want to enter into the cabal of cabal dependency management just to install regular expression support,
I wrote the parsing using list primitives, mostly `span`. Not having regexp support in the standard library is
a bit of a downer. But maybe I just did not find it. Overall, I do not have a good first impression of the
Haskell library documentation, or its feature set.

## Day 5 - Supply Stacks

I am beginning to see the implications of lazy evaluation. And I am wondering how I will fare once the
AoC gets tougher on algorithmic complexity later. Because I do not have a good feeling for where Haskell builds
up thunks, as can be observed in my SML-like function definitions...

## Day 6 - Tuning Trouble

Doing a bit more reading about Haskell yesterday I understood that tail recursion is not necessary in Haskell,
and that the benefit of lazy evaluation is that formulating recursion like in the `startMarker` function
does not carry extra cost. In many cases it works to just write down the mathematical solution. So today's
problem was easy to solve in Haskell, as it was just about a list prefix containing unique characters.

## Day 7 - No Space Left on Device

This took me long. Because of functional programming perfectionism. Instead of simply writing down the
correct calculation, I tried to "optimize" it while doing it. The old trap.

## Day 8 - Treetop Tree House

I learned a bit about all the helpful list functions Haskell has. I like how `transpose` and `zipWith` allow
me to express the calculation in a readable manner.