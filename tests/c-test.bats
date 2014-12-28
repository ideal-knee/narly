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

@test "declares functions" {
    result="$(echo '(declare-fn (int foo) ((int bar) (char** baz)))' | sbcl --script src/cli.lisp)"
    [ "$result" == "int foo(int bar, char** baz);" ]
}

@test "defines functions" {
    result="$(echo '(defn (int foo) ((int bar) (char** baz)) (qux bar baz) (quux bar))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'int foo(int bar, char** baz){\n  qux(bar, baz);\n  quux(bar);\n}' ]
}

@test "supports infix operators" {
    result="$(echo '(+ 1 2 3)' | sbcl --script src/cli.lisp)"
    [ "$result" == "(1)+(2)+(3)" ]
}

@test "supports nested infix operators" {
    result="$(echo '(+ 1 2 (/ 3 4))' | sbcl --script src/cli.lisp)"
    [ "$result" == "(1)+(2)+((3)/(4))" ]
}

@test "allows variable declaration" {
    result="$(echo '(declare-var int count)' | sbcl --script src/cli.lisp)"
    [ "$result" == "int count" ]
}

@test "allows variable declaration with initial value" {
    result="$(echo '(declare-var int count :init 0)' | sbcl --script src/cli.lisp)"
    [ "$result" == "int count=0" ]
}

@test "allows array declaration" {
    result="$(echo '(declare-var int count :length 5)' | sbcl --script src/cli.lisp)"
    [ "$result" == "int count[5]" ]
}

@test "allows array declaration without explicit length" {
    result="$(echo '(declare-var int count :array? true)' | sbcl --script src/cli.lisp)"
    [ "$result" == "int count[]" ]
}

@test "allows external variable declaration" {
    result="$(echo '(declare-var int count :external? true)' | sbcl --script src/cli.lisp)"
    [ "$result" == "extern int count" ]
}

@test "allows global variable declaration" {
    result="$(echo '(declare-global-var int count)' | sbcl --script src/cli.lisp)"
    [ "$result" == "int count;" ]
}

@test "allows array declaration with initial value" {
    result="$(echo '(declare-var int count :length 5 :init (1 2 3 4 5))' | sbcl --script src/cli.lisp)"
    [ "$result" == "int count[5]={1, 2, 3, 4, 5}" ]
}

@test "allows variable assignment" {
    result="$(echo '(set count 0)' | sbcl --script src/cli.lisp)"
    [ "$result" == "count = 0" ]
}

@test "has while control structure" {
    result="$(echo '(while (< foo bar) (baz foo))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'while ((foo)<(bar)) {\n  baz(foo);\n}' ]
}

@test "has for control structure" {
    result="$(echo '(for ((set i 0) (< i 10) (set i (+ i 1))) (foo i))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'for (i = 0; (i)<(10); i = (i)+(1)) {\n  foo(i);\n}' ]
}

@test "allows symbolic constants" {
    result="$(echo '(def count 0)' | sbcl --script src/cli.lisp)"
    [ "$result" == $'#define count 0' ]
}

@test "allows prefix increment operator" {
    result="$(echo '(inc count)' | sbcl --script src/cli.lisp)"
    [ "$result" == $'++(count)' ]
}

@test "allows postfix increment operator" {
    result="$(echo '(inc-after count)' | sbcl --script src/cli.lisp)"
    [ "$result" == $'(count)++' ]
}

@test "allows prefix decrement operator" {
    result="$(echo '(dec count)' | sbcl --script src/cli.lisp)"
    [ "$result" == $'--(count)' ]
}

@test "allows postfix decrement operator" {
    result="$(echo '(dec-after count)' | sbcl --script src/cli.lisp)"
    [ "$result" == $'(count)--' ]
}

@test "supports and operator" {
    result="$(echo '(and a b)' | sbcl --script src/cli.lisp)"
    [ "$result" == "(a&&b)" ]
}

@test "supports or operator" {
    result="$(echo '(or a b)' | sbcl --script src/cli.lisp)"
    [ "$result" == "(a||b)" ]
}

@test "supports when" {
    result="$(echo '(when foo (bar baz) (qux quux))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'if (foo) {\n  bar(baz);\n  qux(quux);\n}' ]
}

@test "supports cond with one clauses" {
    result="$(echo '(cond (foo (bar baz)))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'if (foo) {\n  bar(baz);\n}' ]
}

@test "supports cond with two clauses" {
    result="$(echo '(cond (foo (bar baz) (corge)) (qux (bar quux)))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'if (foo) {\n  bar(baz);\n  corge();\n}\nelse if (qux) {\n  bar(quux);\n}' ]
}

@test "supports cond with else clause" {
    result="$(echo '(cond (foo (bar baz)) (:else (bar quux)))' | sbcl --script src/cli.lisp)"
    [ "$result" == $'if (foo) {\n  bar(baz);\n}\nelse {\n  bar(quux);\n}' ]
}

@test "support array reference" {
    result="$(echo '(aref foo 12)' | sbcl --script src/cli.lisp)"
    [ "$result" == "foo[12]" ]
}
