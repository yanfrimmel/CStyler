# CStyler

C source code style formatter tool

## Features

* Fromat files form OO style code (camel cases to separate) to c style code (under scores to separate words).
* Fromat files form c style code (under scores to separate words) to OO style code (camel cases to separate).
* Apply to entire directory of files and recusive within a directory.

## How to use

### To build

```bash
    stack build
```

### To C style code from OO style code

```bash
    stack exec cstyler-exe -- -c $path
```

### To OO style code from c style code

```bash
    stack exec cstyler-exe -- -o $path
```
