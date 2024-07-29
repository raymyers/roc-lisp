# Roc Lisp

Simple Lisp interpretor in [Roc](https://www.roc-lang.org) based on Peter Norvig's [Python implementation](https://norvig.com/lispy.html).

This is meant as an educational example, for a full-featured Lisp implementation try Racket or SBCL.

## Requirements
* [Roc](https://www.roc-lang.org) (tested with nightly 070d14a5d60)

## Test
```
roc test
```

## Run
Run an interactive REPL:
```
roc dev
```
Exit with Ctrl-D (EOF).

## Examples
```
(list 1 2 3)
(quote a)
(define n 5)
(set! n 6)
```

```
(define max (lambda (a b) (if (< a b) b a)))

(max 3 4)
```
=> 4

## Features
Values supported:

* Integers (Represented by Roc's I32)
* Lists
* t (true)
* nil (false / empty list)
* lambda (anonymous functions)
* symbols (e.g. `(quote mysym)`)

## Next steps?

These are missing but should be easy to implement.

* Floats
* Quote notation (`'(a b)` expands to `(quote (a b)`)
* Comments
