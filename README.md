This is the code for exercises at 'The type theory behind the GHC internals' leap workshop at LambdaConf 2018 (Boulder, CO).

To work with the code on your system, you need either:

* [Stack](http://haskellstack.org)
* [A Minimal GHC installation](https://www.haskell.org/downloads)
* [The Haskell Platform](https://www.haskell.org/platform/)


## Using Stack

### Building

```
stack build
```

### Running

```
stack exec <executable> [ -- <arguments>]
```
For example:

```
stack exec stlc -- test.f
```

### Exploring in GHCi

```
stack ghci <module file>
```
## Using Cabal sandbox

### Building

```
cabal sandox init
cabal install --only-dependencies
cabal configure
cabal build
```

### Running

```
cabal run <executable> [ -- <arguments>]
```

For example:

```
cabal run stlc -- test.f
```

### Exploring in GHCi

```
cabal repl <executable>
```

For example:

```
cabal repl stlc
```

To work with particular module, you have to load it in GHCi with `:load`.
