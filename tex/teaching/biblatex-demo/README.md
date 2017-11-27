# Basic demonstration of `biblatex-chicago`

The file `sample.bib` is a made-up sample citation database using Biblatex
commands. 
The file `sample.tex` is a demonstration of the basic commands needed for using
`biblatex-chicago`.

The demonstration assumes you have a full installation of TeXLive from
<http://www.tug.org/>.

## Compilation

To compile, save the files in a directory, go to that directory in the terminal,
and run a `latex` engine and `biber` as needed.
A full session would probably look like this:

    ~$ cd ~/Documents/biblatex-demo
    ~$ pdflatex sample
    ~$ biber sample
    ~$ pdflatex sample
    ~$ !!
    ~$ xpdf sample.pdf

But you can make this shorter and tidier with `latexmk`:

    ~$ cd ~/Documents/biblatex-demo
    ~$ latexmk -outdir=aux -pdf -bibtex sample
    ~$ mv aux/sample.pdf ./
    ~$ xpdf sample.pdf
