;;
;; misc.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module contains miscellaneous helper functions, macros, etc.
;;

#lang racket

;; -- Requires --

(require (for-syntax syntax/parse))

;; -- Provides --

(provide

 ;; -- Macros --

 ;; Starts a thread.
 thread-start

 ;; Anaphoric if macro.
 ifmap

 ;; -- Procedures --

 (contract-out

  ;; Creates a syntax object containing a symbol with the specified format.
  [make-syntax/symbol (->* (syntax? string?) #:rest list? syntax?)]

  ;; Converts seconds to milliseconds.
  [seconds->milliseconds (-> real? real?)]

  ;; Converts milliseconds to seconds.
  [milliseconds->seconds (-> real? real?)]

  ;; Equivalent to (or/c arg false?).
  [maybe/c (-> contract? contract?)]

  ;; Returns an empty string.
  [string-empty (-> string?)]

  ;; Predicate returning #t if the argument is an empty string.
  [string-empty? (-> string? boolean?)]))

;; -- Public Macros

(define-syntax (thread-start stx)
  (syntax-parse stx
    [(_ body:expr ...+)
     (syntax
      (thread (thunk body ...)))]))

(define-syntax (ifmap stx)
  (define-syntax-class binding-pair
    #:description "binding pair"
    (pattern (name:id value:expr)))
  (syntax-parse stx
    [(_ (formals:binding-pair ...+) body ...+)
     #`(let (formals ...)
         (if (and formals.name ...)
             (begin
               body ...)
             #f))]))

;; -- Public Procedures --

(define (make-syntax/symbol stx fmt . v)
  (datum->syntax stx (string->symbol (apply format fmt v))))

(define (seconds->milliseconds seconds)
  (* seconds 1000.0))

(define (milliseconds->seconds milliseconds)
  (/ milliseconds 1000.0))

(define (maybe/c ctc)
  (or/c ctc false?))

(define string-empty
  (const ""))

(define (string-empty? str)
  (zero? (string-length str)))
