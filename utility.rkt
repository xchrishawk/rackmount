#lang racket

;; -- Provides --

(provide thread-start)
(provide with-semaphore)

;; -- Macros --

(define-syntax-rule (thread-start expr0 expr ...)
  (thread
   (λ ()
     expr0
     expr ...)))

(define-syntax-rule (with-semaphore sema expr0 expr ...)
  (call-with-semaphore
   sema
   (λ ()
     expr0
     expr ...)))
