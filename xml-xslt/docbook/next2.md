---
title: My Article
author: Andrew Cashner
documentclass: ms
...

# Abstract

This is an article with sections, including different kinds of floats and
whatnot with cross references and bibliography.

# Introduction

This is the introduction. It will not remind you of the preface to
*Moby-Dick*. 
Or perhaps it will. 
"Call me a document" [@Kircher:Musurgia, 1].
It's up to you.^[Follow the hermeneutic circle all the way to the end.]

What you'll see when you look at figure \ref{fig:squares} is a lot of squares.
They're all nested inside each other and get infinitely smaller.
I made it in LaTeX using a loop. 
You'll find it is different from what I will show you in section \ref{sec:next}.

![Infinite squares](img/squares.jpg){#fig:squares}

# Next part { #sec:next }

What's different about these squares from the next thing I'll talk about is
that next thing isn't squares at all. 
It's notated music (example \ref{ex:crotchets}).
It shows *crotchets*, which is the British term for quarter notes, I think. 
Anyway, Benjamin Britten in *Billy Budd* would like you to make sure they keep
"gently flowing." 
They don't have anything to do with *crocheting*, which is a handicraft not
frequently featured in British opera.^[But why not?]

::: { .example }
\begin{example}
\label{ex:crotchets}
\includegraphics{img/crotchets.png}
\caption{``Gently flowing crotchets''}
\end{example}
:::

# References
