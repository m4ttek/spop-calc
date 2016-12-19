# Setup
Install 'The Haskell Tool Stack' from https://docs.haskellstack.org/en/stable/README/
```
stack setup
```
Install tools
```
stack install hlint
```
```
stack install stylish-haskell
```
```
stack install ghc-mode
```
# Run project (interactive mode)
```
stack ghci
```
# Tests
```
stack test
```
# Web application
```
cd web
```
Install yesod framework
```
stack build yesod-bin cabal-install --install-ghc
```
Build libraries
```
stack build
```
Start "JRebel" like development
```
stack exec -- yesod devel
```
View site at http://localhost:3000/
