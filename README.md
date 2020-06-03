# cl-aristid

Draw Lindenmayer Systems with Common LISP!

![](examples/crystal_005.pbm)

```
sudo apt install sbcl
```

```
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
sbcl --load quicklisp.lisp
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
```

```
sbcl
* (pushnew (truename ".") asdf:*central-registry*)
* (ql:quickload "cl-aristid")

* (cl-aristid:dragon 3)
```
