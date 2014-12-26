Narly
=====

**N**ot **a** **r**eal **l**anguage... **y**et.

Narly is a preprocessor to generate code (just C at the moment) from
Lisp s-expressions.  It gives you the full power of Common Lisp at
compile time.

Usage
-----

    cat your-narly-source-file.n | sbcl --script src/cli.lisp > your-c-source-file.c

Should work fine with any ANSI-compliant Common Lisp, though figuring
out the exact incantation to get the functionality of the SBCL
`--script` option is left as an exercise of the user.  *If you don't
have a preference of Common Lisp implementation, just use
[SBCL](http://www.sbcl.org/).*

### Run tests ###

Requires [Bats](https://github.com/sstephenson/bats).

    bats tests/*.bats

Why isn't this implemented in Clojure?
--------------------------------------

Gross.

License
-------

Copyright © 2014 Dan Kee
