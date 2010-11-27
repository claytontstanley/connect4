all: unitTest

compile:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil compile.txt nil

unitTest:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil connect4.txt nil

