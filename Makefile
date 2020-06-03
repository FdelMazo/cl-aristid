all:
	sbcl --load "examples.lisp"
	sbcl --load "examples-extra.lisp"
	mkdir -p examples
	for f in *.pbm; do ffmpeg -y -i "$$f" "$${f%.pbm}.png" -hide_banner; done
	rm *.pbm
	mv *.png examples
