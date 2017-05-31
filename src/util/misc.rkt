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


;; -- Procedures --

(define (wrapped-thread-receive-evt)
  (wrap-evt
   (thread-receive-evt)
   (λ (evt)
     (thread-receive))))
