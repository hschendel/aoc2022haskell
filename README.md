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

## Day 9 - Rope Bridge

Even though I still fight the syntax sometimes, learning Haskell is bearing fruits. The decomposition of
a problem into pure functions allowed me to easily generalize the solution for part two. The important thing
was not to be misled into simulating the whole map but just considering the positions of all knots.

## Day 11 - Monkey in the Middle

It turns out that managing more complex data structures (array of monkeys) was a bit of a challenge for me.
I am not sure if this is the idiomatic way. Also, the modulo solution to "manage the worry level" was a bit
of a guess after seeing that all test divisors are prime.

## Day 12 - Hill Climbing Algorithm

My vacation is over and it shows. I also did not get at first that going down more than one height level is OK.
That in turn led me to doubt my implementation. Finally the A* performance does not look too bad for a pure
functional implementation.