all: unitTest

unitTest:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil connect4.txt nil

