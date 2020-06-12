# cl-aristid

Draw Lindenmayer Systems with Common LISP!

<img src="./examples/crystal_005.svg" width="400">

- [Introduction](#Introduction)
- [Example](#Example)
- [Interface](#Interface)
- [Getting started with Common LISP](#GettingstartedwithCommonLISP)

## 1. <a name='Introduction'></a>Introduction

## 2. <a name='Example'></a>Example

You can read the [examples.lisp](examples.lisp) file to see the code for several fractals, or just run `make` to see them in action.

Drawing the Dragon Curve with `cl-aristid`

1. First, we want to enter SBCL (just write `sbcl` in the terminal) and load this system

```
(pushnew (truename ".") asdf:*central-registry*)
(ql:quickload "cl-aristid")
```

2. After the call to `use-package` we now have access to the symbols exposed by the package, detailed in [Interface](#Interface).

```
(use-package 'cl-aristid)
```

3. `defaristid`

We now want to define the different aristids of our fractal. We are calling an 'aristid' to each symbol on our Lindenmayer alphabet that does _something_, that is, to any drawing function. If we check the [Dragon Curve definition](https://en.wikipedia.org/wiki/L-system#Example_6:_Dragon_curve), we have 3 aristids (F, + and -)

```
(defaristid F :len 2) ; The letter F draws forward a line. The reason we use 2 as length is explained in the Interface section
(defaristid - :angle 90) ; The - will turn left 90 degrees
(defaristid + :angle -90) ; The + will turn right 90 degrees
```

4. `defrule`

After our aristids, we want to define the production rules. This rules are the ones that will rewrite our function string on each iteration. The Dragon Curve has only 2 rules: `(X → X + Y F +)` and `(Y → − F X − Y)`. This means that on each iteration we will replace `X` with `X + Y F +` and the same happens to `Y`.

```
; We wrap every rule in a LISP list, to use as an argument later
(defparameter dragon-rules
	(list (defrule X -> (X RIGHT Y F RIGHT))
		  (defrule Y -> (LEFT F X LEFT Y))))
```

5. `make-fractal`

We have almost everything to define our fractal! Remember, a L-system consists of three things: the alphabet of symbols (our aristids), the production rules, and an axiom, which is the first string to be rewritten. In the Dragon Curve example, the axiom is `(F X)`

We want to call `make-fractal` with all of this attributes.

```
(defparameter dragon (make-fractal :name "dragon"
								   :rules dragon-rules
								   :axiom '(F X)))
```

6. `draw`

We are now ready to draw! We just call the `draw` function that receives our fractal and the N iterations we want

```
(draw fractal 10)
```

7. Result

<img src="./examples/dragon_010.svg" width="300">

## 3. <a name='Interface'></a>Interface

## 4. <a name='GettingstartedwithCommonLISP'></a>Getting started with Common LISP

1. Install a Common LISP interpreter

```

sudo apt install sbcl

```

2. Install QuickLisp, the Common LISP library manager

```

curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp

- (quicklisp-quickstart:install)
- (ql:add-to-init-file)

```

3. Enter SBCL from within the repo root and load this package

```

sbcl

- (pushnew (truename ".") asdf:_central-registry_)
- (ql:quickload "cl-aristid")

```

4. You have now available the exposed function of this library, detailed above. You can use them by calling `(cl-aristid:function args)` within SBCL.

Quicktip:

Install `sudo apt install rlwrap` and add `alias sbcl="rlwrap sbcl "` to your `.bashrc` or `.zshrc` to have history search (CTRL+R - UP arrow - DOWN arrow) inside SBCL
