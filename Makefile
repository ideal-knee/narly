# Example usage

default :
	make hello-world

hello-world :
	cat examples/hello.n | sbcl --script src/cli.lisp > gen/hello.c
	cc gen/hello.c -o gen/hello
	gen/hello

test :
	bats tests/*.bats

clean :
	rm gen/*
