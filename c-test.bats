# Narly core tests

@test "includes C files" {
    result="$(echo '(c-include stdio)' | sbcl --script cli.lisp)"
    [ "$result" == '#include "stdio.h"' ]
}

@test "defines functions" {
    result="$(echo '(defn (int foo) ((int bar) (char** baz)) (qux bar baz))' | sbcl --script cli.lisp)"
    echo $result
    [ "$result" == $'int foo(int bar, char** baz) {\n  qux(bar, baz);\n}' ]
}

