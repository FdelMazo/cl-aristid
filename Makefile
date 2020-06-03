all:
	sbcl --load "examples.lisp"
	sbcl --load "examples-extra.lisp"
	mkdir -p examples
	for f in *.pbm; do ffmpeg -y -i "$$f" "$${f%.pbm}.png" -hide_banner; done
	mv *.png examples

clean:
	rm -f *.pbm
	rm -rf examples
