# calculator

The goal of this project is to parse and solve mathematical queries.

## Project

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

Algebra:

- [ ] variable assignment `x = 1`
- [ ] solving with context `2x` (`x = 1`)
- [ ] solving for variable `2x = 1`
- [ ] simplifying expression `1x + 6x`
- [ ] multi variable solving, with context `1x + 2y` (`x = 1`, `y = 2`)

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
