---
date: 2017/10/30
...

You can include files in Markdown (or anywhere) using the `m4` preprocessor's
`include` macro:

To include file `sec1.md1` in the file `main.md`, just write this:

    include(`sec1.md')

Then run the preprocessor: 

    m4 file.md > temp.md
    pandoc -o temp.pdf temp.md

If it's in another directory, use the `-I` option with `m4`.

Here's how to do it all together in one:

    m4 -I sub main.md | pandoc -o main.pdf
