![CI](https://github.com/alessandrocandolini/optimal-picture-url/workflows/CI/badge.svg) [![codecov](https://codecov.io/gh/alessandrocandolini/optimal-picture-url/branch/main/graph/badge.svg?token=W59QP5D1AA)](https://codecov.io/gh/alessandrocandolini/optimal-picture-url)

# optimal-picture-url

Simple exercise to extract from a collection of pictures at different resolutions, the picture with the best resolution according to a given screen width. "Best" resolution here is defined as the resolution closest to the desired one (using the standard Euclidean distance between widths). 

It's essentially just a call to `sortWith`. Nevertheless, it allows to illustrate few important points common in FP design: 
* **property-driven design** and validation through property-based testing (PBT); we will experience a case where PBTs are able to detect errors that are unlikely to be caught by traditional example-based testing techniques; this exercise will also serve as an un example of the fact that PBTs are win-win: they lead to a better design (principled, lawful contracts) and they are more powerful and more expressiveness, while at the same time they don't  introducing conceptual overhead (quite the opposite instead, they remove accidental complexity from how we write tests). 
* **reasoning with types**: computations in context, optionality if and only if the input data is empty, information-preserving guarantees for free from the Functor laws by lifting the business logic to the `Maybe` monad at the edge, non-empty lists, etc. 
* **parametric polymorphism to have properties for free** (no need to write certain PBTs because they are automatically guarantee by purity and parametric polymorphism) 


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
stack exec blahblah-exe
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
