# CStyler

C source code style formatter tool

## How to use

### To build

```bash
    stack build
```

### To C style code from OO style code

```bash
    stack exec cstyler-exe c $filepath
```

### To OO style code from c style code

```bash
    stack exec cstyler-exe oo $filepath
```

## Features

* Form OO style code (camel cases to separate) to c style code (under scores to separate words).
* Form c style code (under scores to separate words) to OO style code (camel cases to separate).
* TODO: Apply to entire directory of *.c files and recusive within a directory.
