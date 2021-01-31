![CI](https://github.com/alessandrocandolini/optimal-picture-url/workflows/CI/badge.svg)

# optimal-picture-url

Simple exercise to extract from a collection of pictures at different resolutions, the picture with the best resolution according to a given screen width. 

It's essentially just a call to `sortWin` :) but it allows to talk about several concepts in FP:
* PBTs (we will see how they will detect errors for us; PBT are a win-win: they allow us to think in terms of properties, which typically leads to a stronger API, they offer more expressiveness, and at the same time they are more powerful, without introducing too much complexity) 
* signatures (eg, empty vs non-empty collection, optionality as lifting, etc) 
* parametric polymorphism to have properties for free (no need to write a PBT) 


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
To run the executable,
```
stack exec optimal-url-exe
```
For a faster feedback loop,
```
stack test --fast --file-watch
```
To run `ghci` (with a version compatible with the resolver) run
```
stack ghci
```
For more information, refer to the `stack` official docs.
