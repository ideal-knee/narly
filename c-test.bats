# Narly core tests

@test "includes C files" {
    result="$(echo '(c-include stdio)' | sbcl --script cli.lisp)"
    [ "$result" == '#include "stdio.h"' ]
}

@test "defines functions" {
    result="$(echo '(defn (int foo) ((int bar) (char** baz)) (qux bar baz))' | sbcl --script cli.lisp)"
    [ "$result" == $'int foo(int bar, char** baz) {\n  qux(bar, baz);\n}' ]
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

