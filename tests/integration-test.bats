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

@test "builds character-counter correctly" {
    cat examples/character-counter.n | sbcl --script src/cli.lisp > gen/character-counter.c
    cc gen/character-counter.c -o gen/character-counter
    result="$(echo $'a b c d e 0 1 2 0' | gen/character-counter)"
    [ "$result" == "digits = 2 1 1 0 0 0 0 0 0 0, white space = 9, other = 5" ]
}

@test "builds print-longest-line correctly" {
    cat examples/print-longest-line.n | sbcl --script src/cli.lisp > gen/print-longest-line.c
    cc gen/print-longest-line.c -o gen/print-longest-line
    result="$(echo $'foo\nbarbar\nqux\nquux' | gen/print-longest-line)"
    [ "$result" == "barbar" ]
}
