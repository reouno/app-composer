# app-composer

app-composer integrates apps and runs. permitted I/O of all apps is only json formatted text for now.

## Environment

- stack Version 1.6.3
- ghc version 8.2.2

## Installation

```
git clone git@github.com:reouno/app-composer.git
cd app-composer
stack build
```

# Usage

run sample modules

```
stack runghc src/Run.hs -- -g "./sample_apps/reader/read.hs >>= ./sample_apps/wakati/wakati.py >>= ./sample_apps/bow/bow.py" -i '{"filePath":"./sample_apps/sample.txt"}'
```
