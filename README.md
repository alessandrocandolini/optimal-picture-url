![CI](https://github.com/alessandrocandolini/optimal-picture-url/workflows/CI/badge.svg)

# optimal-picture-url

Simple exercise to extract from a collection of pictures at different resolutions, the picture with the best resolution according to a given screen width. 

It's essentially just a call to `sortWith`. Nevertheless, it allows to illustrate few important points common in FP design: 
* **property-driven design** and validation through property-based testing (PBT); PBTs will detect errors that are likely unexpected in this exercise; the exercise also serves an un example of the fact that PBTs are win-win: they lead to a better design (principled, lawful contracts) and they are more powerful, and at the same time they more expressiveness, without introducing conceptual overhead (quite the opposite instead, they remove accidental complexity from the tests). 
* **reason with types**: for example, we will see that the output type is a `Maybe` just because the input list can be empty, and the output value is `Nothing` **if nad only if** the input list is empty; this can be easily represented by writing the core function in terms of a non-empty list and using lifting. 
* **parametric polymorphism to have properties for free** (no need to write certain PBTs becuase they are automatically guarantee by purity and parametric polymorphism) 


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
