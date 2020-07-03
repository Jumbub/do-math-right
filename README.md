# 🖩 do-math

Solving mathemetical queries without any implicit floating point innacuracies.

## How it's achieved,

#### Don't represent numbers as floats, (duh)

##### Store numbers as fractions `numberator / denominator`

An expression of `3 + 2` is calculated as `(3/1) + (2/1)`

A user input of `0.2` is converted to a fractional representation `(2/10)`

The fractional representation of any number `n`, with `d` decimal points is converted using `(n * 10^d) / (10^d)`.

##### Only create decimal numbers at display time

The decimal value of an expression is only evaluated for the sake of human readability

Evaluating the decimal value of a fraction _can_ produce innacuracies, therefore if we delay production of that value, we can limit the spread of that innaccuracy.

<br>

#### Be clear about the decimal accuracy of a result,

##### All results have an associated `accuracy`

An expression such as `3 + 2` will have a `exact` accuracy because both operands, `3` and `2` are rational numbers, and the operator `+` produces an exact result.

An expression such as `1 / (10^10000)` will not have an `exact` accuracy, because although all numbers and operations in this expression _can_ produce a rational number, the decimal conversion of this number will not be exact.

All expressions containing irrational numbers like `pi`, or functional approximations like `sin` generate values with an `accuracy`

<br>

#### Replay operations to achieve higher accuracies,

If an expression resolves to an `accuracy` level under your `requested accuracy`, it will increase the decimal precision at which it operates at to increase the accuracy.

An expression `SIN(PI)` may initially resolve to an accuracy of `5` decimal points. But if you request an accuracy of 7 decimal points, it will increase the decimal accuracy of `PI` and the number of iterations by the `SIN` approximation.

<br>

## Goals

Arithmetic:

- [x] whole numbers `100`
- [x] basic operators `+` `-` `/` `*`
- [x] scoping operators `(` `)`
- [x] decimal numbers `1.0`
- [ ] constants `PI`
- [ ] functions `SIN(180)`

Modes:

- [ ] angle units `IN_RADIANS(..)` `IN_DEGREES(..)`
- [ ] answer type `AS_FRACTION(..)` `AS_DECIMAL(..)`
- [ ] decimal accuracy `TO_DECIMAL_ACCURACY(10, ..)`

Algebra:

- [ ] variable assignment `x = 1`
- [ ] solving with context `2x` (`x = 1`)
- [ ] solving for variable `2x = 1`
- [ ] simplifying expression `1x + 6x`
- [ ] solving for variable, with context `2x + 2y` (`x = 1`)
- [ ] solving for variable, with no context `2x = 2y`

Interactivity:

- [x] input and output loop
- [ ] use facts from previous result e.g. `ANS` or `x = 1`

Display:

- [ ] show steps taken to find solution
- [ ] output in Latex syntax
- [ ] expression syntax errors

Parsing:

- [ ] parsing Latex syntax

Research:

- [ ] Can we determine the required accuracy of irrational numbers or function approximations _before_ generating the first result
- [ ] How do we a number in the state of "plus or minus" (e.g. solving for `x` in `|x| = 1`)

<br>

## Input syntax

- Whitespace is ignored
- Negative numbers `(-1)` must be wrapped in parentheses
   - otherwise there is ambiguity: `-5^2` could be `-(5^2) = -25` or `(-5)^2 = -25`
- Variables `x` are all lower case
- Constants `PI` are all upper case
- Functions `POW(2, 10)` are all upper case, and arguments are wrapped in parentheses and comma seperated

<br>

## Development

### Software Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/)

### Building locally

`stack install`

`stack build`

### Testing locally

`stack test`
