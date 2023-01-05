# Cambridge Pseudocode to Python

## Intro

This is a translator written in Haskell that translate a pseudocode source file
into a Python script. 

The adopted pseudocode language is from GCSE and Advanced Level's
computer science textbook and 9618 syllabus.

## How to Use?

### Compile

1. Install [Glasgow Haskell Compiler](https://www.haskell.org/ghc/).
2. Run `compile.sh` on UNIX or `compile.cmd` on Windows.
3. An executable `campseudo-to-py` will be generated.

### Translate Pseudocode

#### UNIX

```sh
./campseudo-to-py file_path
```

#### Windows

```cmd
.\campseudo-to-py.exe file_path
```

e.g. If the source file is `hello.whatever`,
then `hello.whatever.py` will be generated.

### Run Pseudocode

#### UNIX

```sh
./run.sh file_path
```

#### Windows

```cmd
.\run.cmd file_path
```

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
- [x] `WHILE DO ENDWHILE` / `WHILE ENDWHILE`
- [x] `REPEAT UNTIL`
- [x] `FOR TO NEXT` / `FOR TO ENDFOR`
- [x] `FOR TO STEP NEXT` / `FOR TO STEP ENDFOR`
- [ ] `CASE OF OTHERWISE ENDCASE`

### Subroutine

Due to the severe side effect caused by `BYREF`, `BYREF` will not be
implemented. See [Data Type](#data-type) for default passing method.

- [x] `PROCEDURE ENDPROCEDURE`
- [x] `FUNCTION ENDFUNCTION`

### Pointer

Too many side effect! No pointers allowed!

### File

- [x] `OPEN FOR` / `OPENFILE FOR`
- [x] `READFILE`
- [x] `WRITEFILE`
- [x] `EOF`
- [x] `CLOSEFILE`

### Built-in Functions

- `CHR`
- `ASC`
- `LCASE`
- `UCASE`
- `TO_UPPER`
- `TO_LOWER`
- `LENGTH`
- `LEFT`
- `RIGHT`
- `MID`
- `NUM_TO_STRING`
- `RAND`
