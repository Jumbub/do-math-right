# do-math

Solving mathemetical queries without any floating point innacuracies.

## Project

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

### Things to think about

- [ ] How do you get the most accurate results possible when performing multiple operations which only give approximate results (e.g. `sine`)
- [ ] How do you handle a case where the operator cannot produce a single operand output (e.g. `x + 1`)
- [ ] How do you handle solving a function which causes one side to be "plus or minus" (e.g. `|x| = 1`)

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
