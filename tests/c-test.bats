# Narly core tests

@test "includes C files" {
    result="$(echo '(c-include stdio)' | sbcl --script src/cli.lisp)"
    [ "$result" == '#include "stdio.h"' ]
}

@test "supports typed names" {
    result="$(echo '(typed-name (int foo-count))' | sbcl --script src/cli.lisp)"
    [ "$result" == "int foo_count" ]
}

@test "supports bodies" {
    result="$(echo '(body (foo bar) (qux bar baz))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'{\n  foo(bar);\n  qux(bar, baz);\n}' ]
}

@test "defines functions" {
    result="$(echo '(defn (int foo) ((int bar) (char** baz)) (qux bar baz) (quux bar))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'int foo (int bar, char** baz) {\n  qux(bar, baz);\n  quux(bar);\n}' ]
}

@test "supports infix operators" {
    result="$(echo '(+ 1 2 3)' | sbcl --script src/cli.lisp)"
    [ "$result" == "(1+2+3)" ]
}

@test "supports nested infix operators" {
    result="$(echo '(+ 1 2 (/ 3 4))' | sbcl --script src/cli.lisp)"
    [ "$result" == "(1+2+(3/4))" ]
}

@test "allows variable declaration" {
    result="$(echo '(declare-var (int count))' | sbcl --script src/cli.lisp)"
    [ "$result" == "int count" ]
}

@test "allows variable declaration with initial value" {
    result="$(echo '(declare-var (int count) 0)' | sbcl --script src/cli.lisp)"
    [ "$result" == "int count = 0" ]
}

@test "allows variable assignment" {
    result="$(echo '(set count 0)' | sbcl --script src/cli.lisp)"
    [ "$result" == "(count = 0)" ]
}

@test "has while control structure" {
    result="$(echo '(while (< foo bar) (baz foo))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'while ((foo<bar)) {\n  baz(foo);\n}' ]
}

@test "has for control structure" {
    result="$(echo '(for ((set i 0) (< i 10) (set i (+ i 1))) (foo i))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'for ((i = 0); (i<10); (i = (i+1))) {\n  foo(i);\n}' ]
}

@test "allows symbolic constants" {
    result="$(echo '(def count 0)' | sbcl --script src/cli.lisp)"
    [ "$result" == $'#define count 0' ]
}

