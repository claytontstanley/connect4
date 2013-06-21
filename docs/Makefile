platform = $(shell uname | tr '[:upper:]' '[:lower:]')

all: htmlDocs

htmlDocs:
	sbcl --noinform --noprint --disable-debugger --load ../LETF/letf.lisp $(platform) htmlConfig.txt nil

