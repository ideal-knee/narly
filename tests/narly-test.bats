# Narly core tests

@test "outputs string literals with double quotes" {
    result="$(echo '"some string"' | sbcl --script src/c-cli.lisp)"
    [ "$result" == '"some string"' ]
}

@test "outputs symbols verbatim" {
    result="$(echo asdf | sbcl --script src/c-cli.lisp)"
    [ "$result" == "asdf" ]
}

@test "translates hyphens in symbol names to underscores" {
    result="$(echo asdf-fdsa | sbcl --script src/c-cli.lisp)"
    [ "$result" == "asdf_fdsa" ]
}

@test "doesn't translate subtraction operators to underscores" {
    result="$(echo '(- asdf-fdsa fdsa-asdf)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(asdf_fdsa)-(fdsa_asdf)" ]
}

@test "outputs numeric values verbatim" {
    result="$(echo 123.45 | sbcl --script src/c-cli.lisp)"
    [ "$result" == "123.45" ]
}

@test "transforms s-expressions into C-like function calls" {
    result="$(echo '(foo-corge bar baz qux-grault)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "foo_corge(bar, baz, qux_grault)" ]
}

@test "renders forms with default context and separator" {
    result="$(echo '(narly-render () (foo bar) (baz qux))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "foo(bar)baz(qux)" ]
}

@test "renders forms with specified context and separator" {
    result="$(echo '(narly-render (:context "{~a}" :separator "; ") (foo bar) (baz qux))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "{foo(bar); baz(qux)}" ]
}

@test "supports Common Lisp character literals" {
    result="$(echo '#\a' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "'a'" ]
}

@test "supports space character literal" {
    result="$(echo '#\Space' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "' '" ]
}

@test "supports newline character literal" {
    result="$(echo '#\Newline' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "'\n'" ]
}

@test "supports tab character literal" {
    result="$(echo '#\Tab' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "'\t'" ]
}

@test "supports constants with terminal characters" {
    result="$(echo '123ul' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "123ul" ]
}

@test "supports floats with decimals" {
    result="$(echo '1.23' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "1.23" ]
}

@test "supports simple floats with exponent" {
    result="$(echo '1.2e-2' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "0.012" ]
}

@test "supports more complex floats with exponent" {
    result="$(echo '-1.23e-5' | sbcl --script src/c-cli.lisp)"
    echo $result
    [ "$result" == "-1.23e-5" ]
}

@test "supports Common Lisp syntax for octal and hexadecimal literals" {
    result="$(echo '31 #o37 #x1F' | sbcl --script src/c-cli.lisp)"
    echo $result
    [ "$result" == "313131" ]
}
