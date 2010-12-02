
all: test

ccl:
	ccl64 --load letf.lisp -- nil unitTests.txt nil

test:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil unitTests.txt nil
