;;
;; thread-util.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines utility functions for working with threads.
;;

#lang racket

;; -- Requires --

(require (for-syntax syntax/parse))

;; -- Provides --

(provide

 ;; Macro for launching a thread.
 thread-start)

;; -- Macros --

(define-syntax (thread-start stx)
  (syntax-parse stx
    [(_ body:expr ...+)
     (syntax
      (thread (thunk body ...)))]))
