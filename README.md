# `do-math-right`

With this app you can solve expressions without any <strong>floating point innaccuracies</strong> or <strong> implicit approximations</strong>.

<br>

## Conventional calculators are liars

<br>

##### They lie about the accuracy of irrational numbers!

Calculate ["pi"](https://www.google.com/search?q=pi), and you will be given an answer of `3.14159265359`

But everyone knows that Pi has more than 11 decimal places?!

<br>

##### They even lie about rational numbers!

Calculate ["10 / 3"](https://www.google.com/search?q=10/3), and you will be told that the answer is exactly `3.33333333333`

But everyone knows that the 3 should recur forever, not just 11 times?!

<br>

##### Heck, they even lie about whole numbers!

Calculate ["10^20 + 1"](https://www.google.com/search?q=10^20%2b1), and you will be told that the answer is exactly `1e+20`

But `1e+20 = 10^20`. Where did my `+ 1` go?

Did this calculator just ignore me?!

<br>

## `do-math-right` is not a liar!

<br>

##### Doing irrational numbers right!

```nothing
$ do-math-right
>  PI
=> 3.1415926536 ± 1/10000000000
```

The symbol `±` denotes `plus or minus`, which is an explicit annotation of the accuracy of the result.

Therefore if your project only requires an accuracy of `0.0001` units, you can use this result with confidence.

<br>

##### Doing rational numbers right!

```nothing
$ do-math-right
>  10/3
=> 3.(3)
```

The decimal `(3)` denotes `3 recurring`, which describes an infinitely repeating pattern in the decimal.

Therefore regardless of the accuracy of your project, you can use this result with confidence.

<br>

##### Doing whole numbers right!

```nothing
$ do-math-right
>  10*10*10*10*10*10*10*10*10*10*10*10*10*10*10*10*10*10*10*10+1
=> 100000000000000000001
```

The numbers in this application are not stored as traditional fixed size floating points.

Therefore this calculator can _theoretically_ perform operations accurately on numbers infinitely large.

<br>

## How does it work?

### Numbers are never represented as floats, (duh)

##### Every number is represented as a fraction `numerator / denominator`

All numbers are converted to their fractional representations using the formula `(n * 10^d) / (10^d)`, where `n` is the decimal number, and `d` is the number of decimal places.

- `3 + 2` becomes `(3/1) + (2/1)`
- `0.2` becomes `(2/10)`

##### Every number can be infinitely large

Numbers in this app can be infinitely large because they are not constrained by the conventional 64 bit size constraints of a floating point in hardware.

<br>

### Inaccuracies are made explicit,

##### Every number has an associated `accuracy`

Every number `x` has an associated accuracy `y` where `x ± y`.

A result of `x ± y` implies that the exact result lies between `n - x` and `n + x`.

- Whole numbers such as `2` would be `2 ± 0` (which simplifies to `2`)
- Approximations such as `Pi` would be `3.14 ± 1/100`

##### Innaccurate decimal representations of numbers are only created for display purposes

The app never operates on decimals, it is only used as an _optional_ method of representing the result.

- `10 / 3` represented decimally is `3.(3)`
- `22 / 7` represented decimally is `3.14285 ± 1/100000`

<br>

<br>

### Approximate calculations are replyed until desired accuracy is reached,

All irrational numbers (Pi) and approximate functions (sine) cannot be represented fractionally and therefore have innaccuracies, and when those approximations are operated on they can rapidly become very inaccurate. So to achieve higher accuracies the solver will repeat your calculation with increasingly accurate approximations until the requirement is met.

E.g. `PI * 100` may internally resolve to `314.159 ± 1/1000`, because we only generate Pi to 5 DP. But if our accuracy requirement is `1/100000`, this result will not satisfy us. So the solver will re-evaluate with a more accurate approximation of Pi, it achieves our desired result of `314.159 ± 1/100000`.

<br>

## Input syntax

- Whitespace is ignored
- Negative numbers `(-1)` must be wrapped in parentheses
    - otherwise there is ambiguity: `-5^2` could be `-(5^2) = -25` or `(-5)^2 = -25`
- Decimal numbers between `1` and `-1` must precede with `0`
    - the number `.2` will fail to be parsed, where the number `0.2` will succeed
- Constants `PI` are all upper case
- Functions `SIN(1)` are all upper case, and arguments are wrapped in parentheses

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
- [x] constants `PI`
- [x] functions `SIN(180)`

Modes:

- [x] answer type `APPROXIMATE(1)` `APPROXIMATE(0)`
- [x] required accuracy `SETDP(10)`
- [ ] angle units `IN_RADIANS(..)` `IN_DEGREES(..)`

Interactivity:

- [x] input and output loop
- [ ] use last result with `ANS`
- [ ] arrow key support

Display:

- [ ] output in Latex syntax
- [ ] expression syntax errors

Research:

- [ ] Can we determine the required accuracy of irrational numbers or function approximations _before_ generating the first result
