.PHONY: vendor clean binary-sbcl binary-ccl binary

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Clean -----------------------------------------------------------------------
clean:
	rm -rf bin

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')

bin:
	mkdir -p bin

binary-sbcl: bin
	sbcl --load "src/build.lisp"

binary-ccl: bin
	ccl --load "src/build.lisp"

binary: binary-sbcl

bin/brows: $(lisps) $(assets) Makefile
	make binary-sbcl
