;;
;; misc.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines miscellaneous helper functions and macros.
;;

#lang racket

;; -- Requires --

(require (for-syntax syntax/parse))

;; -- Provides --

(provide

 ;; Macro for starting a thread.
 thread-start

 ;; Macro for lambda taking any number of arguments and ignoring all of them.
 λ0

 (contract-out

  ;; Procedure wrapping (thread-receive-evt) so that the synchronization result
  ;; is the value returned by (thread-receive).
  [wrapped-thread-receive-evt (-> evt?)]))

;; -- Macros --

(define-syntax (thread-start stx)
  (syntax-parse stx
    [(_ body:expr ...+)
     (syntax
      (thread (λ () body ...)))]))

(define-syntax (λ0 stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax
      (λ unused
        body ...))]))

;; -- Procedures --

(define (wrapped-thread-receive-evt)
  (wrap-evt
   (thread-receive-evt)
   (λ (evt)
     (thread-receive))))
