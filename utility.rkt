#lang racket

;; -- Provides --

(provide thread-start)

;; -- Macros --

(define-syntax-rule (thread-start expr0 expr ...)
  (thread
   (λ ()
     expr0
     expr ...)))
