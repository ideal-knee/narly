# Narly core tests

@test "includes C files" {
    result="$(echo '(c-include stdio)' | sbcl --script cli.lisp)"
    [ "$result" == '#include "stdio.h"' ]
}

@test "supports typed names" {
    result="$(echo '(typed-name (int foo-count))' | sbcl --script cli.lisp)"
    [ "$result" == "int foo_count" ]
}

@test "supports bodies" {
    result="$(echo '(body (foo bar) (qux bar baz))' | sbcl --script cli.lisp)"
    [ "$result" == $'{\n  foo(bar);\n  qux(bar, baz);\n}' ]
}

@test "defines functions" {
    result="$(echo '(defn (int foo) ((int bar) (char** baz)) (qux bar baz) (quux bar))' | sbcl --script cli.lisp)"
    echo $result
    [ "$result" == $'int foo (int bar, char** baz) {\n  qux(bar, baz);\n  quux(bar);\n}' ]
}

@test "supports infix operators" {
    result="$(echo '(+ 1 2 3)' | sbcl --script cli.lisp)"
    [ "$result" == "(1+2+3)" ]
}

@test "supports nested infix operators" {
    result="$(echo '(+ 1 2 (/ 3 4))' | sbcl --script cli.lisp)"
    [ "$result" == "(1+2+(3/4))" ]
}

@test "allows variable declaration" {
    result="$(echo '(declare-var (int count))' | sbcl --script cli.lisp)"
    [ "$result" == "int count" ]
}

