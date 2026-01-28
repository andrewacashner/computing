out_dir = build
dirs = $(out_dir)

ts_in = $(wildcard *.ts)
js_out = $(addprefix $(out_dir)/, $(ts_in:%.ts=%.js))

html_in = $(wildcard *.html)
css_in = $(wildcard *.css)

html_out = $(addprefix $(out_dir)/, $(html_in))
css_out = $(addprefix $(out_dir)/, $(css_in))

.PHONY: js all view clean

all: js $(html_out) $(css_out)

js: $(js_out)

$(out_dir)/%.js : %.ts
	tsc

$(dirs) :
	mkdir -p $(dirs)

$(out_dir)/%.html: %.html | $(dirs)
	cp -u $< $@

$(out_dir)/%.css: %.css | $(dirs)
	cp -u $< $@

view:
	xdg-open $(html_out)

clean:
	rm -rf $(dirs)
