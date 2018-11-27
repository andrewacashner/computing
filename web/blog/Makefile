target = build/index.html
boilerplate = $(shell find src/ -type f -name "*".html)
posts = $(shell find posts/ -type f -name "*".md -print0 | sort -zr | xargs -r0)

all : $(target)

$(target) : $(boilerplate) $(posts)
	pandoc -s \
	    -t html5 \
	    --toc --toc-depth=1 \
	    -B src/top.html \
	    -A src/bottom.html \
	    -c style.css \
	    -o $(target) \
	    $(posts)
