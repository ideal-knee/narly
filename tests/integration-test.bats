# Integration tests

@test "builds hello-world correctly" {
    cat examples/hello.n | sbcl --script src/cli.lisp > gen/hello.c
    cc gen/hello.c -o gen/hello
    result="$(gen/hello)"
    [ "$result" == "hello world" ]
}

@test "builds temp-table correctly" {
    cat examples/temp-table.n | sbcl --script src/cli.lisp > gen/temp-table.c
    cc gen/temp-table.c -o gen/temp-table
    result="$(gen/temp-table)"
    [ "$result" == "$(cat tests/temp-table.txt)" ]
}

@test "builds pipe correctly" {
    cat examples/pipe.n | sbcl --script src/cli.lisp > gen/pipe.c
    cc gen/pipe.c -o gen/pipe
    result="$(echo 'I can pipe stuff!' | gen/pipe)"
    [ "$result" == 'I can pipe stuff!' ]
}

@test "builds wc correctly" {
    cat examples/wc.n | sbcl --script src/cli.lisp > gen/wc.c
    cc gen/wc.c -o gen/wc
    result="$(echo $'hello world\ngoodbye c' | gen/wc)"
    [ "$result" == "2 4 22" ]
}
