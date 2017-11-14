---
bibliography:   master.bib
csl:            chicago-fullnote-bibliography.csl
geometry:       margin=1in
...

# Test of Markdown Footnote and Citation Methods

This is the text.^[This is an inline footnote.]

This is the text.[^fn1]

[^fn1]: This is a footnote made with a forward reference link.

Now I will cite a book [@Cashner:WLSCM32].

I'll do it with a "quotation" [@Cashner:PhD].

Here is a repeated citation [@Cashner:WLSCM32].

I'll put a citation with a quotation 
[See @Cashner:PhD: "Villancicos as musical theology."].

I'll put it in a forward reference *with* brackets around the whole reference.[^Baker3]

[^Baker3]: [See @Illari:Polychoral for more information.]
<!--- Correct: "See <citation> for more" -->

Same deal but with ibid.[^Baker4]

[^Baker4]: [See @Illari:Polychoral for more information.]
<!--- Correct: "See ibid. for more" -->



## Don't do

I'll put a citation with a quotation but no period in the reference 
[@Cashner:PhD: "Villancicos as musical theology"]. 
<!--- no: period after quote in fn -->

I'll put a citation in a footnote.^[@Baker:PerformancePostColonial]
<!--- no: period after citation in footnote -->


I'll put it in a forward reference with no brackets in the reference.[^Baker]

[^Baker]: See @Baker:PerformancePostColonial for more information.
<!--- NO, results in "See ibid., for more" -->

I'll put it in a forward reference *with* brackets around just the cite key.[^Baker2]

[^Baker2]: See [@Baker:PerformancePostColonial] for more information.
<!--- No, puts period after citation. -->

I'll put a citation with a quotation and a period in the reference, as a
footnote.^[@Cashner:PhD: "Villancicos as musical theology."]
<!--- No, puts period after reference -->

I'll put a citation with a quotation and a period in the reference, as a
footnote with forward reference.[^diss]

[^diss]: [@Cashner:PhD: "Villancicos as musical theology."]
<!--- No, lowercase ibid -->

Same deal but without end punctuation in reference.[^Illari1]

[^Illari1]: [@Illari:Polychoral, 34--45]
<!--- No, lowercase ibid and no end period -->




# Bibliography
