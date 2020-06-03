all:
	sbcl --load "examples.lisp"
	sbcl --load "examples-extra.lisp"
	mkdir -p examples
	for f in *.svg; do mogrify -density 500 -background white -fill white -trim -gravity center -format png "$$f"; done
	rm *.svg
	mv *.png examples
