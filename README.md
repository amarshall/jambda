# jambda

A portmanteau of “JAM” and “lambda”, this is JAM’s Lisp. This is an exercise in writing a Lisp interpreter, and isn’t meant to be a “real” language.

## Usage

    ruby -Ilib bin/jambda
    jambda> (1 2 3)
    (1 2 3)

## Internal code style

This is not your father’s Ruby. This is Ruby written by someone who knows idiomatic Ruby, but is experimenting with more purely functional Ruby. As such, expect avoidance of objects and state, replaced instead with an abundance of class methods and freezing.

One naming convention used throughout the code is `nast` and `ntokens`; this is meant to be shorthand for “new AST” and “new tokens”.

## Credits & License

Copyright © 2015 J. Andrew Marshall. All rights reserved.
