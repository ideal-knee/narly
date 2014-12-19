# Narly core tests

@test "outputs string literals with double quotes" {
    result="$(echo '"some string"' | sbcl --script src/cli.lisp)"
    [ "$result" == '"some string"' ]
}

@test "outputs symbols verbatim" {
    result="$(echo asdf | sbcl --script src/cli.lisp)"
    [ "$result" == "asdf" ]
}

@test "translates hyphens in symbol names to underscores" {
    result="$(echo asdf-fdsa | sbcl --script src/cli.lisp)"
    [ "$result" == "asdf_fdsa" ]
}

@test "doesn't translate subtraction operators to underscores" {
    result="$(echo '(- asdf-fdsa fdsa-asdf)' | sbcl --script src/cli.lisp)"
    [ "$result" == "(asdf_fdsa-fdsa_asdf)" ]
}

@test "outputs numeric values verbatim" {
    result="$(echo 123.45 | sbcl --script src/cli.lisp)"
    [ "$result" == "123.45" ]
}

@test "transforms s-expressions into C-like function calls" {
    result="$(echo '(foo bar baz qux)' | sbcl --script src/cli.lisp)"
    [ "$result" == "foo(bar, baz, qux)" ]
}

@test "renders forms with default context and separator" {
    result="$(echo '(narly-render () (foo bar) (baz qux))' | sbcl --script src/cli.lisp)"
    echo $result
    [ "$result" == "foo(bar)baz(qux)" ]
}

@test "renders forms with specified context and separator" {
    result="$(echo '(narly-render (:context "{~a}" :separator "; ") (foo bar) (baz qux))' | sbcl --script src/cli.lisp)"
    echo $result
    [ "$result" == "{foo(bar); baz(qux)}" ]
}

