# TL;DR Just keep using LaTeX

(2022/02/02)

Here I tried out ConTeXt for a grant application. 

# Conclusions
- The ConTeXt project is very cool

## BUT
- The documentation is a thicket of briars
- Online support is spotty, does not keep up with updates
- You have to do a lot yourself in a combination of Plain-ish TeX, new ConTeXt
  macros, and Lua
- *As usual when we try to leave LaTeX*: Bibliography formatting is
  unsatisfactory. There is a Chicago author-date mode but it has a lot of
  errors and I have no clue how to begin fixing it. No footnote-based style at
  all. 
- It does read a BibTeX database but you have to redefine BibLaTeX commands,
  and there are differences between biblatex and bibtex that matter (e.g.,
  series number vs. volume).
- It does output XML in various formats (raw XML, XHTML, HTML), but not in a
  way that you could easily use to, say, convert to DOCX. You would need more
  DIY XSLT transformations.

- If you want to start a publishing company it would be worth looking into as
  a backend for typesetting an XML-based workflow.
- For authoring it offers *no clear advantages* over LaTeX in terms of syntax
  or structure.
- `make4ht -f odt [FILE]` produces a good ODT file from most LaTeX documents
  without problems. 

- tables, diagrams, etc. will always be a problem, but also each
  journal/publisher will deal with them differenty
- if you need to manipulate data, use Excel/Calc (or a database if you really
  need it). If you need to display data in a PDF, just use LaTeX.

- For simple documents, use Markdown or LaTeX; or even Plain TeX or Opmac if
  you really feel the need.
- For things destined directly for the web, use HTML/CSS if you need control,
  have specific style/design interests, or really care about the resulting
  code (don't); otherwise Markdown will get you there.
- **For anything with a bibliography, the only workable option for my needs is
  LaTeX, as we have found many, many times.**

- Keep bibliography, tables, diagrams, etc. in separate files; use consistent
  semantic markup; avoid anything exotic or if absolutely needed, contain it
  in a separate file. **Just use LaTeX.**

