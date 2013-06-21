
all: test

ccl:
	ccl64 --load letf.lisp -- nil unitTests.txt nil

test:
	sbcl --noinform --noprint --disable-debugger --load ../LETF/letf.lisp nil unitTests.txt nil
