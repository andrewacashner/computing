# notes2web

- A shell script that takes a set of files written in Markdown, and convert them
  into a complete website (tree of linked HTML pages).
- Generates an index.html page with a table of contents and add links back to
  the index.html page on each individual page.
- Generates an index of keywords using the `kwindex` C program.

## Usage

    sh notes2web.sh [INPUT_DIRECTORY] [OUTPUT_DIRECTORY]

## System Requirements

- A C compiler to compile `kwindex.c` (`gcc`, `clang`)
  * This should compile and run on any Unix-based system, including Mac OSX
  * It has been tested on Fedora Linux and Mac OSX
- A shell to run `notes2web.sh` and/or `test.sh` (`bash`, `dash`)
  * On Mac OSX, open `Terminal` app
- The program `pandoc`, available for multiple platforms at <http://www.pandoc.org/>
- A text editor for creating your own Markdown files (`vim`, `emacs`, etc.)
- A web browser for viewing the results (`elinks`, `firefox`, etc.)

# kwindex

- Takes one or more Markdown files, searches them for a section of keywords, and
  then extracts the keywords and the filenames and creates an alphabetized index
  in Markdown format showing which files the keywords appeared in.
- Keywords should be included in source files as shown below. The section is
  preceded with the heading line `# Keywords`, and the keywords are separated by
  semicolons. 

``` 
# Keywords

one; two; three, four
``` 

- `kwindex` will take this input file and produce this output file. Note that it
  changes the file extensions to what `notes2web` will generate (html, not md).

```
# Index of Keywords

---------- ---------
four       [input1](input1.html)
one        [input1](input1.html)
three      [input1](input1.html)
two        [input1](input1.html)
---------- ---------
```

## Usage

`notes2web` will automatically call `kwindex` with the right arguments, but if
you want to run it by itself, call it like this:

    ./kwindex -o [OUTPUT_DIR] [INPUT_FILES]

## Compilation

Just the command `make` will compile `kwindex.cx` from the source files.

# test.sh

A simple test script is provided to run the commands with the included test
files and display the result in a browser.
This requires `xdg-open`, so will only work on Linux systems.
