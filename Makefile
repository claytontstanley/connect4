all: connect4

compile:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil compile.txt nil

connect4:
	sbcl --noinform --noprint --disable-debugger --load letf.lisp nil connect4.txt nil

