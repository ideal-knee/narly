# Narly core tests

@test "includes C files" {
    result="$(echo '(c-include stdio)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == '#include "stdio.h"' ]
}

@test "supports typed names" {
    result="$(echo '(typed-name (int foo-count))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int foo_count" ]
}

@test "allows array typed names" {
    result="$(echo '(typed-name (int foo-count :array? true))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int foo_count[]" ]
}

@test "supports bodies" {
    result="$(echo '(body (foo bar) (qux bar baz))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'{\n  foo(bar);\n  qux(bar, baz);\n}' ]
}

@test "declares functions" {
    result="$(echo '(declare-fn (int foo) ((int bar) (char** baz)))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int foo(int bar, char** baz);" ]
}

@test "defines functions" {
    result="$(echo '(defn (int foo) ((int bar) (char** baz)) (qux bar baz) (quux bar))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'int foo(int bar, char** baz){\n  qux(bar, baz);\n  quux(bar);\n}' ]
}

@test "supports infix operators" {
    result="$(echo '(+ 1 2 3)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(1)+(2)+(3)" ]
}

@test "supports nested infix operators" {
    result="$(echo '(+ 1 2 (/ 3 4))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(1)+(2)+((3)/(4))" ]
}

@test "allows variable declaration" {
    result="$(echo '(declare-var int count)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int count" ]
}

@test "allows variable declaration with initial value" {
    result="$(echo '(declare-var int count :init 0)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int count=0" ]
}

@test "allows array declaration" {
    result="$(echo '(declare-var int count :length 5)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int count[5]" ]
}

@test "allows array declaration without explicit length" {
    result="$(echo '(declare-var int count :array? true)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int count[]" ]
}

@test "allows external variable declaration" {
    result="$(echo '(declare-var int count :external? true)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "extern int count" ]
}

@test "allows signed variable declaration" {
    result="$(echo '(declare-var int count :qualifiers (signed))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "signed int count" ]
}

@test "allows const variable declaration" {
    result="$(echo '(declare-var int count :qualifiers (const))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "const int count" ]
}

@test "allows short variable declaration" {
    result="$(echo '(declare-var int count :qualifiers (short))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "short int count" ]
}

@test "allows multiple qualifiers on variable declaration" {
    result="$(echo '(declare-var int count :qualifiers (unsigned long))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "unsigned long int count" ]
}

@test "allows global variable declaration" {
    result="$(echo '(declare-global-var int count)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int count;" ]
}

@test "allows array declaration with initial value" {
    result="$(echo '(declare-var int count :length 5 :init (1 2 3 4 5))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "int count[5]={1, 2, 3, 4, 5}" ]
}

@test "allows variable assignment" {
    result="$(echo '(set count 0)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "count = 0" ]
}

@test "has while control structure" {
    result="$(echo '(while (< foo bar) (baz foo))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'while ((foo)<(bar)) {\n  baz(foo);\n}' ]
}

@test "has for control structure" {
    result="$(echo '(for ((set i 0) (< i 10) (set i (+ i 1))) (foo i))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'for (i = 0; (i)<(10); i = (i)+(1)) {\n  foo(i);\n}' ]
}

@test "allows symbolic constants" {
    result="$(echo '(def count 0)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'#define count 0' ]
}

@test "allows prefix increment operator" {
    result="$(echo '(inc count)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'++(count)' ]
}

@test "allows postfix increment operator" {
    result="$(echo '(inc-after count)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'(count)++' ]
}

@test "allows prefix decrement operator" {
    result="$(echo '(dec count)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'--(count)' ]
}

@test "allows postfix decrement operator" {
    result="$(echo '(dec-after count)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'(count)--' ]
}

@test "supports and operator" {
    result="$(echo '(and a b)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(a)&&(b)" ]
}

@test "supports or operator" {
    result="$(echo '(or a b)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(a)||(b)" ]
}

@test "supports bitwise and operator" {
    result="$(echo '(bit-and a b)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(a)&(b)" ]
}

@test "supports bitwise or operator" {
    result="$(echo '(bit-or a b)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(a)|(b)" ]
}

@test "supports bitwise xor operator" {
    result="$(echo '(bit-xor a b)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(a)^(b)" ]
}

@test "supports bitwise left shift operator" {
    result="$(echo '(bit-shift-left a b)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(a)<<(b)" ]
}

@test "supports bitwise right shift operator" {
    result="$(echo '(bit-shift-right a b)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(a)>>(b)" ]
}

@test "supports bitwise complement operator" {
    result="$(echo '(bit-complement a)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "~a" ]
}

@test "supports when" {
    result="$(echo '(when foo (bar baz) (qux quux))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'if (foo) {\n  bar(baz);\n  qux(quux);\n}' ]
}

@test "supports cond with one clauses" {
    result="$(echo '(cond (foo (bar baz)))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'if (foo) {\n  bar(baz);\n}' ]
}

@test "supports cond with two clauses" {
    result="$(echo '(cond (foo (bar baz) (corge)) (qux (bar quux)))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'if (foo) {\n  bar(baz);\n  corge();\n}\nelse if (qux) {\n  bar(quux);\n}' ]
}

@test "supports cond with else clause" {
    result="$(echo '(cond (foo (bar baz)) (:else (bar quux)))' | sbcl --script src/c-cli.lisp)"
    [ "$result" == $'if (foo) {\n  bar(baz);\n}\nelse {\n  bar(quux);\n}' ]
}

@test "support array reference" {
    result="$(echo '(aref foo 12)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "foo[12]" ]
}

@test "support octal and hexidecimal constants with terminal characters" {
    result="$(echo '(constant #x123 ul)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "291ul" ]
}

@test "support enumerations" {
    result="$(echo '(enum foo bar (baz 3) qux)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "enum foo { bar, baz = 3, qux };" ]
}

@test "supports not operator" {
    result="$(echo '(not foo)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "!foo" ]
}

@test "supports cast" {
    result="$(echo '(cast foo int*)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "(int*) foo" ]
}

@test "supports addition assignment operator" {
    result="$(echo '(set-add a b)' | sbcl --script src/c-cli.lisp)"
    [ "$result" == "a += b" ]
}
