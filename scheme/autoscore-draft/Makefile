INCLUDES = -I $(LYLOCAL)

all:
	guile voices.scm && lilypond $(INCLUDES) score.ly

clean:
	rm -i score.ly score.pdf
