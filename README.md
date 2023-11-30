[![CI](https://github.com/alessandrocandolini/advent-of-code2023/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/advent-of-code2023/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/advent-of-code2023/graph/badge.svg?token=I1bCJU2hQj)](https://codecov.io/gh/alessandrocandolini/advent-of-code2023) 


# advent-of-code2023

https://adventofcode.com/2023

## Calendar
- Day 1: [problem](https://adventofcode.com/2023/day/1) | [solution](src/Day1.hs) | [test](test/Day1Spec.hs) NOT DONE
- Day 2: [problem](https://adventofcode.com/2023/day/2) | [solution](src/Day2.hs) | [test](test/Day2Spec.hs) NOT DONE
- Day 3: [problem](https://adventofcode.com/2023/day/3) | [solution](src/Day3.hs) | [test](test/Day3Spec.hs) NOT DONE
- Day 4: [problem](https://adventofcode.com/2023/day/4) | [solution](src/Day4.hs) | [test](test/Day4Spec.hs) NOT DONE
- Day 5: [problem](https://adventofcode.com/2023/day/5) | [solution](src/Day5.hs) | [test](test/Day5Spec.hs) NOT DONE
- Day 6: [problem](https://adventofcode.com/2023/day/6) | [solution](src/Day6.hs) | [test](test/Day6Spec.hs) NOT DONE
- Day 7: [problem](https://adventofcode.com/2023/day/7) | [solution](src/Day7.hs) | [test](test/Day7Spec.hs) NOT DONE
- Day 8: [problem](https://adventofcode.com/2023/day/8) | [solution](src/Day8.hs) | [test](test/Day8Spec.hs) NOT DONE
- Day 9: [problem](https://adventofcode.com/2023/day/9) | [solution](src/Day9.hs) | [test](test/Day9Spec.hs) NOT DONE
- Day 10: [problem](https://adventofcode.com/2023/day/9) | [solution](src/Day10.hs) | [test](test/Day10Spec.hs) NOT DONE

...

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/).

Assuming `stack` is installed in the system, the project can be build by running
```
stack build
```
To build and also run the tests, run
```
stack test
```
which is equivalent to
```
stack build --test
```
To run with test coverage
```
stack test --coverage
```
which generates a textual and HTML report.

To run the executable,
```
stack exec advent-of-code2023-exe
```
or passing arguments
```
stack exec advent-of-code2023-exe -- -d <day> -f <filename> 
```

For faster feedback loop,
```
stack test --fast --file-watch
```
To run `ghci` (with a version compatible with the resolver) run
```
stack ghci
```
For more information, refer to the `stack` official docs.
