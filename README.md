Scripts
=======

hc compiles the file.

	ghc -o $1 $1.hs

The file should be named A.hs, B.hs, C.hs, etc.

r runs the file on small input. (ex. r A) On the first (0th) attempt, omit the 2nd parameter. On subsequent attempts, add the number as the second argument. (ex. r A 1)

	num=${2-"0"}
	./$1 "$1-small-attempt$num.in"

rb runs the file on large input.

./$1 "$1-large.in"

Parsing
=======

Basic types

* `int`
* `integer`
* `str`
* `chr`
* `float`
* `double`
* `bool`

Combinators

* `emptyH = line >>`: read a header, discard, and continue. (Ex. the header contains the number of test cases; a lot of the time we can safely discard.
* `tupleS`: take a tuple of parsers and make a parser for the tuple using spaces as the separator
* `tupleL`: take a tuple of parsers and make a parser for the tuple using lines as the separator
* `listS`: read list separated by spaces
* `listL`: read list separated by lines (Warning: this continues until end of file)
* `readH :: (Int -> Int) -> Parser a -> Parser (Int, [a])`: read a header containing an integer, and use it to determine how many times to run `Parser a`.
* `readH_ :: (Int -> Int) -> Parser a -> Parser [a]`: like `readH`, but don't bother outputting the integer.
* `readN`, `readN_` are where you just use `id` for `Int -> Int`.
* `readH' :: (a -> Int) -> Parser a -> Parser b -> Parser (a, [b])` is the more complicated version where you give a parser for the header, and extract a length from it.

Example (2015-2C):

To read
```
6
2
he loves to eat baguettes
il aime manger des baguettes
4
a b c d e
f g h i j
a b c i j
f g h d e
4
...
```
use
```haskell
type Input = [[S]]
type Output = I

input :: String -> [Input]
input = justParse (emptyH $ listL $ readN_ $ listS str)
```
