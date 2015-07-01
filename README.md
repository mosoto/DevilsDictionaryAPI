Haskell web api using the Spock framework for hosting on CloudFoundry.

## BUILD
```
cabal build
```

## RUN
```
export PORT=3000
dist/build/DevilsDictionary/DevilsDictionary
```

## API
- [GET] / 
Return the complete Devil's Dictionary
- [GET] /keys
Return all the dictionary keys
- [GET] /{key}
Return the definition of the specified word
