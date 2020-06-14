# math-ez

The goal of this project is to parse and solve mathematical queries.

## Project

### Ideas

- Should it be able to parse all of Latex
    - Latex is not a strict syntax so scoping could be a problem

### Goals

Arithmetic:

- [ ] whole numbers `100`
- [ ] basic operators `+` `-` `/` `*`
- [ ] scoping operators `(` `)`
- [ ] decimal numbers `1.0`
- [ ] constants `PI`
- [ ] functions `SIN(180)`

Algebra:

- [ ] variable assignment `x = 1`
- [ ] solving with context `2x` (`x = 1`)
- [ ] solving for variable `2x = 1`
- [ ] simplifying expression `1x + 6x`
- [ ] multi variable solving, with context `1x + 2y` (`x = 1`, `y = 2`)

## Development

### Software Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/)

### Building locally

`git clone ...`

`cd math-ez`

`stack install`

`stack build`

### Testing locally

`stack test`
