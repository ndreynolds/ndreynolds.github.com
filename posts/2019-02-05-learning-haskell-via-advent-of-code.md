---
title: "Learning Haskell via Advent of Code"
---

![](/images/aoc.gif)[^mn]

[^mn]:
  {-} My Goblins and Elves do battle in Day 15

Inspired by [this post][1] from Forest Smith on his experience learning Rust
with Advent of Code, I wanted to share my own experience working through the
problems this past December to improve my Haskell skills.

<!--more-->

# Advent of Code

[Advent of Code][2] (AoC) is a collection of holdiay-themed programming puzzles
structured as an Advent calendar---one puzzle is released each day at midnight
from December 1st until the 25th. The puzzles are a bit remniscent of the
[Project Euler][3] problems, but with less math and more fun. The great thing
about AoC is that there's something for everyone. For those who like competitive
programming, there's a leaderboard tracking the 100 fastest users to solve each
day's puzzle. Others don't compete, but just enjoy solving the puzzles or use
them to learn something new.

I decided to use the puzzles as an opportunity to learn some more Haskell.
Haskell and I have had an off-and-on relationship the last several years. The
first time I tried it (in 2014), my mind was blown by functional programming,
pattern matching, type theory, and particularly Cabal "dependency hell"---the
circle of hell that Dante must have forgotten about. Even back then, a part of
me really liked Haskell, but I was just in over my head.

Since then, Elixir has been my gateway drug into real world FP, and I can feel a
lot of Haskell concepts starting to really click in my mind. Also, improvements
to Cabal and new tools like Stack have made the experience smoother for Haskell
beginners.

# My Experience with Haskell

All of my solutions can be found in [my repository][4] on GitHub. Towards the
end, my holiday travels got in the way, so I only managed 18 of the 25 puzzles,
but I'm still quite proud of that.[^1]


[^1]: At first, I was so motivated that I wanted to do them all in [Racket][5],
too, but that proved a bit too ambitious. I think I'll try Racket again next
year.

## Structuring Solutions

I spend so much time fiddling with build tools and manifests at work that I find
it really satisfying to just pass a program directly to the compiler.

I decided to write each day's solution (usually two parts) as a single `DayN.hs`
file that I'd run with `runhaskell` or compile with `ghc`. I generally stuck to
what's included in the GHC distribution (mostly [`base`][6] and
[`containers`][7]) so that the programs were free of additional dependencies.
(I made a few exceptions that I'll mention later.)

```bash
$ runhaskell Day01.hs
```

## Working with I/O

The AoC puzzles have a personalized input file that you need to download and run
against your program. In addition to the input file, there is usually a
simplified example to check the results of your algorithm against. It's a good
fit for the Unix philosophy of having programs exchange data via the standard
streams, as I could just redirect different files to the program's stdin, as
well as use pipes for some light preprocessing. So most of the programs are run
like this:

```bash
$ ./Day25 < 25.txt
420
```

Haskell's Prelude provides a very elegant abstraction around the processing of
standard IO streams: `interact :: (String -> String) -> IO ()` ([docs][8]). You just provide
a function that accepts stdin as a `String` and returns another `String` to be
written to stdout, and it handles the rest. I used variations of this pattern in
most solutions:

```haskell
main :: IO ()
main = interact (show . solve . parse)
```

## Parsing Input: To regex or not to regex

The different puzzles called for different approaches to parsing input. Some of
the inputs were simple enough that `lines` and a little transformation did the
job, whereas others called for a regular expression or a real parser. (Sometimes
the parsing was even the biggest challenge.)

Many in the Haskell community tend to prefer parser combinators like [Parsec][9]
and its derivatives for jobs that programmers in other languages would use a
regular expression for. One sign of this (or maybe the reason) is that you'll
find Parsec bundled with GHC, but not any regex libraries.

I actually quite like regexes (short ones, anyway) and I think they can be the
right tool for a lot of jobs. For example, Day 10 required parsing position and
velocity pairs from the following input:

```
position=<-50310,  10306> velocity=< 5, -1>
position=<-20029,  -9902> velocity=< 2,  1>
position=< 10277, -30099> velocity=<-1,  3>
```

Regular expressions are particularly handy for extracting numbers from text with
a lot of extraneous information and whitespace you don't care about.

So I figured out how to use them in Haskell, which is a bit of [a challenge
itself][10]. I actually went in a different direction than the linked post and
used [`Text.Regex.TDFA`][11], which is a new, pure Haskell regex engine.

```haskell
positionRe = "position=<[[:space:]]*(-?[0-9]+),[[:space:]]*(-?[0-9]+)>"
(_, _, _, captures) = str =~ positionRe
```

In other cases, using parser combinators really was the right approach. For
example, in Day 4, my task was to sneak into Santa's prototype suit
manufacturing lab, which involved parsing the "event log" of different elf
guards as they'd start their shifts, fall asleep, wake up, and so on:

```
[1518-11-20 23:58] Guard #1229 begins shift
[1518-11-21 00:51] falls asleep
[1518-11-21 00:53] wakes up
[1518-11-21 00:57] falls asleep
[1518-11-21 00:59] wakes up
[1518-11-22 00:00] Guard #2441 begins shift
[1518-11-22 00:12] falls asleep
[1518-11-22 00:13] wakes up
[1518-11-22 00:17] falls asleep
[1518-11-22 00:46] wakes up
[1518-11-22 00:52] falls asleep
[1518-11-22 00:54] wakes up
```

I'm not saying you couldn't solve this with a regex, but the fact that each
guard can take an arbitrary number of naps would make it very ugly. So I went
with [Megaparsec][12], which I'd already used in another project. It's more or
less the same API as Parsec, but includes some fixes and enhancements that make
it a bit nicer to work with. So far it's been a real pleasure to use.

To parse the event log, I cobbled together the following:

```haskell
data Shift = Shift Integer [ShiftEvent] deriving Show

data ShiftEvent = Asleep Minutes
                | Awake Minutes
                deriving Show

type Minutes = Integer

type Parser = Parsec Void String

minutes :: Parser Minutes
minutes = do
  char '['
  skipSomeTill (digitChar <|> char '-' <|> spaceChar) (char ':')
  min <- count 2 digitChar
  char ']'
  return $ read min

slumber :: Parser [ShiftEvent]
slumber = do
  asleepAt <- minutes
  string " falls asleep\n"
  awakeAt <- minutes
  string " wakes up\n"
  return $ [Asleep asleepAt, Awake awakeAt]

shift :: Parser Shift
shift = do
  _start <- minutes
  string " Guard #"
  guardId <- some digitChar
  string " begins shift\n"
  events <- many (try slumber)
  return $ Shift (read guardId) (concat events)

shifts :: Parser [Shift]
shifts = many shift
```

While the whitespace handling is a bit sloppy here, it has some nice properties.
It's not possible to parse an `Awake` event followed by another `Awake` event.
Parser combinators make it very easy to express a particular order that must be
followed.

## Profiling Slow Code

Most of the puzzles have two parts. It's common to be able to brute force the
solution to Part 1, but to need to step back and think about how to more
efficiently solve Part 2. It's a little bit like this:

* **Part 1**: Compute $factorial(n)$.
* **Part 2**: Compute $factorial(n^1000)$.

In other cases, my initial solution was just plain slow because I'd used the
wrong data structures or missed a critcal bottleneck. For these cases, I used
GHC's [built-in profiling tools][13].

GHC provides two particularly useful profiling options:

* `-prof` - Compiles for cost-center profiling. This means profiling is turned
   on, but adding this alone doesn't define any cost centers. Cost centers are
   just program annotations that flag a particular expression for profiling.
   They can be added manually with `{-# SCC myId #-}`.
* `-fprof-auto` - This option automatically gives all bindings SCC annotations
   (with the exception of those referenced by an `INLINE` pragma), so you don't
   need to add any manually.

At this point, we can compile a program for profiling, but we still need to tell
the runtime system (RTS) to generate the profile when the program is ran. This
can be done (among other ways), via command-line arguments:

* `+RTS` - Demarcates the beginning of the RTS options (which are removed from
  the argv given to your program). Ended with `-RTS` or automatically when the
  end of arguments is reached.
* `-p` - RTS option that produces a time profile (i.e. where is the program spending
  its time?).
* `-h` - RTS option that produces a heap profile (i.e. memory usage).

My [Day 15][14] solution ([source][15]), which produced the gif at the top of
the post, was initially horribly slow.

The puzzle involves creating a simulation of a battle between an army of elves
and an army of goblins in true dungeon crawler style. Each unit on the map has a
sort of deterministic AI. Units each have turns in a pre-defined series, and
each turn they can either attack an enemy in range or move one square towards
the nearest enemy. Finding the nearest enemy is definitely the tough part.

Units cannot move through walls (they're fighting in a cave) and they cannot
move through each other---so the simulation requires finding, for each unit, the
shortest path to an enemy. (In the event of multiple shortest paths, the
directions also define a way to break the tie.) This means calculating the paths
in each round is $O(n^2)$, as we have to calculate the paths between all units
in order to find the shortest ones.

From the puzzle directions:

```
Targets:      In range:     Reachable:    Nearest:      Chosen:
#######       #######       #######       #######       #######
#E..G.#       #E.?G?#       #E.@G.#       #E.!G.#       #E.+G.#
#...#.#  -->  #.?.#?#  -->  #.@.#.#  -->  #.!.#.#  -->  #...#.#
#.G.#G#       #?G?#G#       #@G@#G#       #!G.#G#       #.G.#G#
#######       #######       #######       #######       #######
```

In order to find the shortest paths, I used [Dijkstra's algorithm][16]. My
implementation was correct on small sample inputs, but was too slow to run on my
real input. I knew something was wrong, so I used the profiler to see what my
program was up to:

```bash
$ ghc -prof -fprof-auto Day15.hs
$ ./Day15 +RTS -p < 15.txt
```

Running the second line generates a `Day15.prof` file in the current directory.

By inspecting the report, I found the problem. Dijkstra's algorithm works by
keeping a set of unvisited neighbor nodes and visiting these in order by
shortest distance. I had actually used `Data.Set` to store these unvisited
neighbors and was using `minimumBy` to find the next one to visit.
Unsurprisingly, this was a very hot code path, and my program was spending 99%
of its time there.

A priority queue makes this operation efficient. Elements of the queue are
ranked by their priority---here, by their distance. I used the
[`Data.PSQueue`][17] implementation, which supports $O(1)$ retrieval of the
element with the lowest priority, as well as $O(\log{}n)$ insertion, deletion
and update operations.

My program now produces the following time profile, which is quite reasonable:

```
	Tue Feb  5 20:35 2019 Time and Allocation Profiling Report  (Final)

	   Day15 +RTS -p -hc -RTS

	total time  =       60.23 secs   (60231 ticks @ 1000 us, 1 processor)
	total alloc = 75,047,638,704 bytes  (excludes profiling overheads)

COST CENTRE              MODULE       SRC                               %time %alloc

lookup                   Data.PSQueue Data/PSQueue.hs:(117,1)-(125,37)   33.4   35.4
open                     Main         Day15.hs:127:1-55                  20.3    1.6
insert                   Data.PSQueue Data/PSQueue.hs:(139,1)-(149,49)   12.9   21.0
adjustWithKey            Data.PSQueue Data/PSQueue.hs:(190,1)-(198,62)   12.1   13.0
deleteMin                Data.PSQueue Data/PSQueue.hs:(317,1)-(318,43)    6.2    7.8
dijkstra.update          Main         Day15.hs:(115,3)-(121,31)           6.1    5.9
dijkstra.prune           Main         Day15.hs:(108,3)-(113,33)           1.6    4.2
adjacents                Main         Day15.hs:139:1-67                   1.3    2.2
dijkstra.prune.neighbors Main         Day15.hs:112:5-78                   1.1    2.2
openAdjacents            Main         Day15.hs:142:1-61                   1.1    2.0
dijkstra.initQ           Main         Day15.hs:(99,3)-(100,78)            0.5    1.8
```

The only red flag is the `60.23 sec` it takes to run! I expected some overhead,
but not quite so much. Compiling again without the profiling options and with
`-O2` brings the total runtime down to a few seconds.

## Formatting Code

While it has nothing to do with puzzles, automatic code formatting has become an
important part of my workflow.

This past year, I've been using Elixir's `mix format` (inspired by tools like
`gofmt`) to format all the Elixir code I write. I used to think aligning the
code to make it "pretty" was my job, but now I've realized that's a big waste of time.

Haskell doesn't yet and may never have a canonical formatter included with GHC,
but there are some different packages to fill this need.

I used [brittany][18] to format my solutions and was impressed with the results.
I didn't always agree with it's choices, but I think that's the trade-off when
using a formatter.

[1]: https://www.forrestthewoods.com/blog/learning-rust-via-advent-of-code/
[2]: https://adventofcode.com/
[3]: https://projecteuler.net/
[4]: https://github.com/ndreynolds/advent-of-code
[5]: https://racket-lang.org/
[6]: http://hackage.haskell.org/package/base
[7]: http://hackage.haskell.org/package/containers
[8]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:interact
[9]: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/parsec-3.1.13.0/Text-Parsec.html
[10]: https://gabebw.com/blog/2015/10/11/regular-expressions-in-haskell
[11]: http://hackage.haskell.org/package/regex-tdfa
[12]: http://hackage.haskell.org/package/megaparsec
[13]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
[14]: https://adventofcode.com/2018/day/15
[15]: https://github.com/ndreynolds/advent-of-code/blob/master/2018/haskell/Day15.hs
[16]: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
[17]: http://hackage.haskell.org/package/PSQueue
[18]: https://github.com/lspitzner/brittany