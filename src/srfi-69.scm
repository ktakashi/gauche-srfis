;;; -*- mode: scheme; coding; utf8 -*-
;;;
;;; srfi-69.scm - Basic Hash Table implementation for Gauche
;;;
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module srfi-69
  (use srfi-13)
  (use srfi-114)
  (export (rename srfi:make-hash-table make-hash-table)
	  hash-table? ;; re-export
	  (rename srfi:alist->hash-table alist->hash-table)

	  hash-table-equivalence-function hash-table-hash-function

	  hash-table-ref 
	  (rename hash-table-get hash-table-ref/default)
	  hash-table-set!
	  ;; (rename hash-table-put! hash-table-set!)
	  hash-table-delete! 
	  hash-table-exists? ;; re-export
	  	  
	  (rename srfi:hash-table-update! hash-table-update!)
	  hash-table-update!/default
	  ;; (rename hash-table-update! hash-table-update!/default)

	  hash-table-size
	  ;; (rename hash-table-num-entries hash-table-size)
	  hash-table-keys   ;; re-export
	  hash-table-values ;; re-export
	  hash-table-walk 
	  hash-table-fold   ;; re-export
	  hash-table->alist ;; re-export
	  hash-table-copy   ;; re-export
	  hash-table-merge!

	  hash ;; re-export
	  hash-by-identity 
	  string-hash	 ;; re-export
	  string-ci-hash ;; re-export
	  ))

(select-module srfi-69)

(define srfi:make-hash-table
  (case-lambda
   ((eql? hash) 
    (make-hash-table (make-comparator #t eql? #f hash)))
   ((eql?)
    (cond ((eq? eql? eq?)         (make-hash-table 'eq?))
	  ((eq? eql? eqv?)        (make-hash-table 'eqv?))
	  ((eq? eql? equal?)      (make-hash-table 'equal?))
	  ((eq? eql? string=?)    (make-hash-table 'string=?))
	  ((eq? eql? string-ci=?) (make-hash-table string-ci-comparator))
	  (else (error "make-hash-table: unknown equivalent procedure" eql?))))
   (() (make-hash-table 'equal?))))

(define hash-by-identity eq-hash)

;; bit different thing
(define (hash-table-ref/default ht key default) (hash-table-get ht key default))

;; ref and update
(define no-entry (list 'no-entry))
(define (failure-thunk who key)
  (lambda () (errorf "~a: no association for key" who key)))
(define hash-table-ref
  (case-lambda
   ((ht key thunk)
    (let ((val (hash-table-get ht key no-entry)))
      (if (eq? val no-entry)
	  (thunk)
	  val)))
   ((ht key)
    (hash-table-ref ht key (failure-thunk 'hash-table-ref key)))))

(define srfi:hash-table-update!
  (case-lambda
   ((ht key proc thunk)
    (hash-table-update! ht key
		       (lambda (v)
			 (if (eq? v no-entry)
			     (thunk)
			     (proc v)))
		       no-entry))
   ((ht key proc)
    (srfi:hash-table-update! ht key proc
			     (failure-thunk 'hash-table-update! key)))))

(define (hash-table-walk table proc)
  (hash-table-for-each (lambda (k v) (proc k v)) table))

(define (srfi:alist->hash-table alist . opts)
  (rlet1 ht (apply make-hash-table opts)
    (for-each (lambda (kv)
		(hash-table-update!
		 ht
		 (car kv)
		 (lambda (x) (if (eq? no-entry x) (cdr kv) x))
		 no-entry)) alist)))

(define (hash-table-equivalence-function ht)
  (let1 cmp (hash-table-comparator ht)
    (comparator-equality-predicate cmp)))

(define (hash-table-hash-function ht)
  (let1 cmp (hash-table-comparator ht)
    (comparator-hash-function cmp)))

(define (hash-table-merge! ht1 ht2)
  (hash-table-for-each ht2 (lambda (k v) (hash-table-put! ht1 k v)))
  ht1)

(define string-ci-hash string-hash-ci)

;; for some reason test-module complained
(define hash-table-set! hash-table-put!)
(define hash-table-size hash-table-num-entries)
(define hash-table-update!/default hash-table-update!)