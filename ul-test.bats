# UL Tests

@test "outputs string literals with double quotes" {
    result="$(echo '"some string"' | sbcl --script ul.lisp)"
    [ "$result" == '"some string"' ]
}

@test "outputs symbols verbatim" {
    result="$(echo asdf | sbcl --script ul.lisp)"
    [ "$result" == "asdf" ]
}

@test "outputs numeric values verbatim" {
    result="$(echo 123.45 | sbcl --script ul.lisp)"
    [ "$result" == "123.45" ]
}

@test "transforms s-expressions into C-like function calls" {
    result="$(echo '(foo bar baz qux)' | sbcl --script ul.lisp)"
    [ "$result" == "foo(bar, baz, qux)" ]
}

