- Read input file line by line
- Read each line ascii character at a time
    - If '/' ignore it and all following in this line
    - Tokenize at commas, ignore spaces, copy words into queue
    - Read first word, lookup in list
    - Compare required number/type of arguments to those in queue
        - Error if not right num
        - If right number, then compile:
            { Opcode for instruction word,
              target register,
              source register or address,
              function or immediate }
    - Write output (hex format? or true binary, not ASCII)

# Extended
- Labels as jump/branch targets
- \*+2 instead of line number for jump target
- other addressing modes
