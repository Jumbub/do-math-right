# do-math

The goal of this project is to parse and solve mathematical queries.

## Project

### Things to think about

- [ ] How do you get the most accurate results possible when performing multiple operations which only give approximate results (e.g. `sine`)
- [ ] How do you handle a case where the operator cannot produce a single operand output (e.g. `x + 1`)
- [ ] How do you handle solving a function which causes one side to be "plus or minus" (e.g. `|x| = 1`)

### Goals

Arithmetic:

- [ ] whole numbers `100`
- [ ] basic operators `+` `-` `/` `*`
- [ ] scoping operators `(` `)`
- [ ] decimal numbers `1.0`
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

Display:

- [ ] show steps taken to find solution

Parsing:

- [ ] parsing Latex syntax

## Development

### Software Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/)

### Building locally

`git clone ...`

`cd calculator`

`stack install`

`stack build`

### Testing locally

`stack test`
