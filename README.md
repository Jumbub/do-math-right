# math-ez

The goal of this project is to parse and solve mathematical queries.

## Project

### Ideas

- Should it be able to parse all of Latex
    - Latex is not a strict syntax so scoping could be a problem

### Goals


- [ ] Parse numerical operands, and operators `+` `-` `/` `*` `(` `)`

    - [ ] Solve numerical expression `1 + 1`

    - [ ] Solve numerical expression `1 * (-2)`

    - [ ] Solve numerical expression `(2 + 1) * 3`

<br>

- [ ] Parse variable operands, and operator `simplify`

    - [ ] Simplify single variable expressions `(1 + 2)x + x`

    - [ ] Solve single variable equations `(1 + 2)x + x`

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
