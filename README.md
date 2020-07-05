# `do-math-right`

Solve expressions without any <strong>floating point innacuracies</strong> or <strong> implicit approximations</strong>.

<br>

## Calculators are liars

<br>

##### They lie about the accuracy of irrational numbers!

Calculate ["pi"](https://www.google.com/search?q=pi), you will be told that the answer is exactly `3.14159265359`

But, everyone knows that Pi has more than 11 decimal places?!

<br>

##### They even lie about rational numbers!

Calculate ["10 / 3"](https://www.google.com/search?q=10/3), you will be told that the answer is exactly `3.33333333333`

But, most people know that 10 divided by 3 will produce a decimal with a 3 that recurs forever?!

<br>

##### Heck, they even lie about whole numbers!

Calculate ["10^20 + 1"](https://www.google.com/search?q=10/3), you will be told that the answer is exactly `1e+20`

But, `1e+20 = 10^20`. Where did my `+ 1` go?

Did this calculator just ignore me?!

<br>

## `do-math-right` is not a liar!

<br>

##### Doing irrational numbers right!

Calculating `PI` on _this_ app would result in something _similar_ to: `3.141595 ± 1/200000`

The symbol `±` denotes `plus or minus`, which is an explicit annotation of the accuracy of the result.

Therefore if your project only requires an accuracy of `0.0001` units, you can use this result with confidence.

> The accuracy level computed by do-math-right is defined by the user

<br>

##### Doing rational numbers right!

Calculating `10/3` on _this_ app would result in something similar to: `3.(3)`

The decimal `(3)` denotes `3 recurring`, which describes an infinitely repeating pattern in the decimal.

Therefore regardless of the accuracy of your project, you can use this result with confidence.

<br>

##### Doing whole numbers right!

Calculating `10^20 + 1` on _this_ app would result in the correct answer of `100000000000000000001`.

The numbers in this application are not stored as traditional fixed size floating points.

Therefore this calculator can _theoretically_ perform operations accurately on numbers infinitely large.

<br>

## How does it work?

#### Numbers are never represented as fixed floating points, (duh)

##### Every number is represented as a fraction `numerator / denominator`

All numbers are converted to their fractional representations using the formula `(n * 10^d) / (10^d)`, where `n` is the decimal number, and `d` is the number of decimal places.

`3 + 2` becomes `(3/1) + (2/1)`

`0.2` becomes `(2/10)`

##### Every number can be infinitely large

Numbers in this app can be infinitely large because they are not constrained by the conventional 64 bit size constraints of a floating point in hardware.

<br>

#### Be clear about the decimal accuracy of a result,

##### Every number has an associated `accuracy`

The app can operate on `Exact` numbers, or numbers which are within `Plus or minus x`.

An `Exact` number, implies that the number is perfectly accurate.

A number `n` with an accuracy of `Plus or minus x` implies that the number is between `n - x` and `n + x`.

Each operation will have it's own effect on the resulting numbers accuracy level.

##### Decimal representations of numbers are only created for display purposes

The app never operates on decimals, it is only used as an _optional_ method of representing the result.

`10 / 3` represented decimally is `3.(3)`

`Pi` represented decimally is `3.141595 ± 1/200000`

<br>

<br>

🚧 Details beyond this point are not implemented 🚧

#### Replay operations to achieve higher accuracies,

If an expression resolves to an `accuracy` level under your `requested accuracy`, it will increase the decimal precision at which it operates at to increase the accuracy.

An expression `SIN(PI)` may initially resolve to an accuracy of `5` decimal points. But if you request an accuracy of 7 decimal points, it will increase the decimal accuracy of `PI` and the number of iterations by the `SIN` approximation.

<br>

## Input syntax

- Whitespace is ignored
- Negative numbers `(-1)` must be wrapped in parentheses
    - otherwise there is ambiguity: `-5^2` could be `-(5^2) = -25` or `(-5)^2 = -25`
- Decimal numbers between `1` and `-1` must precede with `0`
    - the number `.2` will fail to be parsed, where the number `0.2` will succeed
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

### Goals

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
