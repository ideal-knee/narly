# Narly core tests

@test "includes C files" {
    result="$(echo '(include c) (c-include stdio)' | sbcl --script narly.lisp)"
    [ "$result" == '#include "stdio.h"' ]
}

@test "defines functions" {
    result="$(echo '(include c) (defn (int foo) ((int bar) (char** baz)) (qux bar baz))' | sbcl --script narly.lisp)"
    echo $result
    [ "$result" == $'int foo(int bar, char** baz) {\n  qux(bar, baz);\n}' ]
}

