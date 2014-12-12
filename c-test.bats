# Narly core tests

@test "includes C files" {
    result="$(echo '(include c) (c-include stdio)' | sbcl --script narly.lisp)"
    [ "$result" == '#include "stdio.h"' ]
}

