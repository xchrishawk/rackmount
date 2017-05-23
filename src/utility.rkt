;;
;; utility.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Provides --

(provide thread-start)
(provide with-semaphore)
(provide
 (contract-out
  [delayed (-> (values (-> any) (-> any/c any)))]
  [list-refs (->* (list?) #:rest (listof exact-nonnegative-integer?) list?)]
  [string-empty? (-> string? boolean?)]
  [length-at-least (-> list? exact-nonnegative-integer? boolean?)]))

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

;; -- Procedures --

(define (delayed)
  (let* (;; Unique object to act as a placeholder
         [placeholder (new object%)]
         ;; Semaphore to block getting value until it's set
         [semaphore (make-semaphore)]
         ;; The value
         [value placeholder]
         ;; Procedure to get value. Will block until it is set.
         [get-value
          (λ ()
            (call-with-semaphore semaphore (λ () value)))]
         ;; Procedure to set value. Can only be called once. Returns the value set.
         [set-value
          (λ (new-value)
            (when (not (eq? value placeholder))
              (error "Delayed value was already set!"))
            (set! value new-value)
            (semaphore-post semaphore)
            new-value)])
    ;; Return the get and set procedures.
    (values get-value set-value)))

(define (list-refs lst . idxs)
  (reverse
   (let loop ([lst lst] [idxs idxs] [idx 0] [result null])
     (cond
       [(null? idxs)
        result]
       [(null? lst)
        (error "Invalid index:" (first idxs))]
       [(equal? idx (first idxs))
        (loop (rest lst) (rest idxs) (add1 idx) (cons (first lst) result))]
       [else
        (loop (rest lst) idxs (add1 idx) result)]))))

;; -- Predicates --

(define (string-empty? str)
  (equal? str ""))

(define (length-at-least lst len)
  (cond
    [(zero? len) #t]
    [(null? lst) #f]
    [else (length-at-least (rest lst) (sub1 len))]))
