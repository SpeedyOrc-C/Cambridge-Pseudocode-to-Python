# Cambridge Pseudocode to Python

## Intro

This is a translator written in Haskell that translate a pseudocode source file
into a Python script. 

The adopted pseudocode language is from GCSE and Advanced Level's
computer science textbook.

## How to Use?

### Compile

1. Install [Glasgow Haskell Compiler](https://www.haskell.org/ghc/).
2. Run `compile.sh`.
3. An executable `campseudo-to-py` will be generated.

### Translate Pseudocode

```sh
./campseudo-to-py <pseudocode_source_code>
```

e.g. If the source file is `hello.whatever`,
then `hello.whatever.py` will be generated.

### Run Pseudocode

1. Install [Python](https://www.python.org/downloads/).
2. Run the following command:
```sh
sh run.sh <pseudocode_source_code>
```
3. It generates the Python script, and it is executed afterwards.

## Language Feature Supported

### Data Type

- [x] `INTEGER` (by value)
- [x] `REAL` (by value)
- [x] `STRING` (by value)
- [x] `CHAR` (by value)
- [x] `BOOLEAN` (by value)
- [x] `ARRAY` (by reference, multi-dimensional)
- [x] `TYPE` (by value, enumerated)
- [x] `TYPE` (by reference, composite)

### Statement

- [x] Assignment
- [x] `DECLARE`
- [x] `INPUT`
- [x] `OUTPUT`
- [x] `RETURN`
- [x] `CALL`

### Control Flow

- [x] `IF THEN ENDIF`
- [x] `IF THEN ELSE ENDIF`
- [x] `WHILE DO ENDWHILE`
- [x] `REPEAT UNTIL`
- [x] `FOR TO NEXT`
- [x] `FOR TO ENDFOR`
- [x] `FOR TO STEP NEXT`
- [x] `FOR TO STEP ENDFOR`
- [ ] `CASE OF OTHERWISE ENDCASE`

### Subroutine

Due to the severe side effect caused by `BYREF`, `BYREF` will not be
implemented. See [Data Type](#data-type) for default passing method.

- [x] `PROCEDURE ENDPROCEDURE`
- [x] `FUNCTION ENDFUNCTION`

### Pointer

Too many side effect! No pointers allowed!

### File

- [ ] `OPEN FOR`
- [ ] `READFILE`
- [ ] `WRITEFILE`
- [ ] `EOF`
- [ ] `CLOSEFILE`

### Built-in Functions
