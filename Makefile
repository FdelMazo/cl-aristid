all:
	sbcl --load "examples.lisp"
	sbcl --load "examples-extra.lisp"
	mkdir -p examples
	mv *.pbm examples
	git add -f examples
