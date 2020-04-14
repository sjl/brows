.PHONY: vendor clean binary-sbcl binary-ccl binary

all: bin/brows

# Clean -----------------------------------------------------------------------
clean:
	rm -rf bin

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')

bin:
	mkdir -p bin

binary-sbcl: bin
	/usr/local/bin/sbcl --noinform --load "src/build.lisp"

binary-ccl: bin
	/usr/local/bin/ccl64 --load "src/build.lisp"

binary: binary-sbcl

bin/brows: $(lisps) Makefile
	make binary-sbcl
	mv brows bin
