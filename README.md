# Teto
![Tetolang Logo](TETO.png)
Teto is a APL Inspired programming language with a prefix syntax and symbols that fit on your keyboard. The following is an example of Teto's syntax
You can download the Teto Interpreter on the release tab.

## Understanding Teto
The following is a code in Teto:
```
> + * 2 |23
=> 552.0
```
This code example can be read like this, in infix form, as an example:
```
foldl (+) 0 (map (*2) [1..23])
```
Teto treats arrays differently from most languages, giving operands a way to work directly with arrays. For example, a `number * array` will map each element of the array times the number. But an `array * number` will give that array multiplied `number` times.
```
* 3 [1,2,3]
[3.0, 6.0, 9.0]

* [1,2,3] 3
[[1.0, 2.0, 3.0], [1.0, 2.0, 3.0], [1.0, 2.0, 3.0]]
```
You can say that operations with numbers and arrays happen in a more "logical form". Please have fun trying out different combinations to see how they work!

### Functions in Teto
Teto has the following functions mapped to special characters:
`'` -> Minimum (`'|10`)
`'` -> Maximum (`"|20`)
`>` -> Reduce (`> + |10`)
`<` -> Map (`< +2 |10`)

Teto is still a work in progress and it's just a hobbie project made for fun. The quality of the interpreter's code is pretty bad, but if you are interested in helping, feel free to do so!
