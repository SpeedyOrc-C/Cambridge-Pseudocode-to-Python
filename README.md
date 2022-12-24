# Cambridge Pseudocode to Python

## Intro

This is a translator written in Haskell that translate a pseudocode source file
into a Python script. 

The adopted pseudocode language is from GCSE and Advanced Level's
computer science textbook.

## Language Feature Supported

### Data Type

- [x] `INTEGER` (by value)
- [x] `REAL` (by value)
- [x] `STRING` (by value)
- [x] `CHAR` (by value)
- [x] `BOOLEAN` (by value)
- [x] `ARRAY` (by reference, multi-dimensional)
- [x] `TYPE` (by value)
- [x] `STRUCT` (by reference)

### Statement

- [x] Assignment
- [x] `DECLARE`
- [x] `INPUT`
- [x] `OUTPUT`
- [ ] `RETURN`
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

- [ ] `PROCEDURE ENDPROCEDURE`
- [ ] `FUNCTION ENDFUNCTION`

### Pointer

Too many side effect! No pointers allowed!

### File

- [ ] `OPEN FOR`
- [ ] `READFILE`
- [ ] `WRITEFILE`
- [ ] `EOF`
- [ ] `CLOSEFILE`

### Built-in Functions


