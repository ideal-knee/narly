# Narly core tests

@test "outputs string literals with double quotes" {
    result="$(echo '"some string"' | sbcl --script cli.lisp)"
    [ "$result" == '"some string"' ]
}

@test "outputs symbols verbatim" {
    result="$(echo asdf | sbcl --script cli.lisp)"
    [ "$result" == "asdf" ]
}

@test "outputs numeric values verbatim" {
    result="$(echo 123.45 | sbcl --script cli.lisp)"
    [ "$result" == "123.45" ]
}

@test "transforms s-expressions into C-like function calls" {
    result="$(echo '(foo bar baz qux)' | sbcl --script cli.lisp)"
    [ "$result" == "foo(bar, baz, qux)" ]
}

