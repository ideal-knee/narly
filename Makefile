# Example usage

default :
	make hello-world temp-table pipe

hello-world :
	cat examples/hello.n | sbcl --script src/cli.lisp > gen/hello.c
	cc gen/hello.c -o gen/hello
	gen/hello

temp-table :
	cat examples/temp-table.n | sbcl --script src/cli.lisp > gen/temp-table.c
	cc gen/temp-table.c -o gen/temp-table
	gen/temp-table

pipe :
	cat examples/pipe.n | sbcl --script src/cli.lisp > gen/pipe.c
	cc gen/pipe.c -o gen/pipe
	echo 'I can pipe stuff!' | gen/pipe

test :
	bats tests/*.bats

clean :
	rm gen/*
