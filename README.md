Gauche SRFIs
============

(Will be) A collection of missing SRFI for Gauche. Following is the list of
supporting missing SRFIs. (and I hope these will be soon supported on Gauche.)

- SRFI-69: Basic hash tables


Requirements
------------

Gauche 0.9.5_pre1 (to use comparator)


To use as R7RS library
----------------------

This library itself doesn't provide R7RS style library so you need to create
files something like this:

    srfi/69.scm

And it's content must be like this (depends on the SRFI):

```
(define-module srfi.69 (extend srfi-69))
```
