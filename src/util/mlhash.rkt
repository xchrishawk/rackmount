;;
;; mlhash.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines a multi-level hash map type (mlhash). Each mlhash has a
;; pre-specified number of "levels", which represents a mapping level in the
;; overall hierarchy.
;;
;; (mlhash-set mlh 0 1 2 'value)
;;   is equivalent to:
;; mlh[0][1][2] = 'value
;;
;; The "remove" and "count" functions can be called with a number of keys fewer
;; than the total number of levels for the mlhash. In that case, they method will
;; be applied recursively on all sub-hashes of the specified key.
;;

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; Constructs a new mlhash instance.
  [rename make-mlhash mlhash (-> exact-positive-integer? mlhash?)]

  ;; Functionally extends mlh by adding a value with the specified keys, overwriting
  ;; any existing mapping, and returning the extended mlhash.
  [mlhash-set (->i ([mlh mlhash?])
                   #:rest [args (mlh) (λ (args)
                                        (and (list? args)
                                             (= (length args) (add1 (mlhash-levels mlh)))))]
                   [result mlhash?])]

  ;; Functionally deletes by removing the value with the specified keys, and
  ;; returning the updated mlhash.
  [mlhash-remove (->i ([mlh mlhash?])
                      #:rest [args (mlh) (λ (args)
                                           (and (list? args)
                                                (<= (length args) (mlhash-levels mlh))))]
                      [result mlhash?])]

  ;; Returns the value for the specified keys in mlh.
  [mlhash-ref (->i ([mlh mlhash?])
                   (#:failure-result [failure-result any/c])
                   #:rest [args (mlh) (λ (args)
                                        (and (list? args)
                                             (= (length args) (mlhash-levels mlh))))]

                   any)]

  ;; Returns the number of items below the node with the specified keys.
  [mlhash-count (->i ([mlh mlhash?])
                     #:rest [args (mlh) (λ (args)
                                          (and (list? args)
                                               (< (length args) (mlhash-levels mlh))))]
                     [result exact-nonnegative-integer?])]))

;; -- Structs --

(struct mlhash (levels root))

;; -- Public Procedures --

(define (make-mlhash levels)
  (mlhash levels (hash)))

(define (mlhash-set mlh . args)
  (struct-copy
   mlhash
   mlh
   [root (let loop ([level (mlhash-levels mlh)]
                    [h (mlhash-root mlh)]
                    [args args])
           (if (zero? level)
               (first args)
               (hash-set h
                         (first args)
                         (loop (sub1 level)
                               (hash-ref h (first args) (hash))
                               (rest args)))))]))

(define (mlhash-remove mlh . args)
  (struct-copy
   mlhash
   mlh
   [root (let loop ([level (mlhash-levels mlh)]
                    [h (mlhash-root mlh)]
                    [args args])
           (cond
             [(null? args) (hash-clear h)]
             [(= 1 level) (hash-remove h (first args))]
             [else
              (let* ([key (first args)]
                     [next-h (hash-ref h key #f)])
                (if (not next-h)
                    h
                    (let ([updated-next-h (loop (sub1 level) next-h (rest args))])
                      (if (hash-empty? updated-next-h)
                          (hash-remove h key)
                          (hash-set h key updated-next-h)))))]))]))

(define (mlhash-ref mlh #:failure-result [failure-result (thunk (error "Not found!"))] . args)
  (let loop ([level (mlhash-levels mlh)]
             [h (mlhash-root mlh)]
             [args args])
    (if (= 1 level)
        (hash-ref h (first args) failure-result)
        (let ([next-h (hash-ref h (first args) #f)])
          (if next-h
              (loop (sub1 level)
                    next-h
                    (rest args))
              (if (procedure? failure-result) (failure-result) failure-result))))))

(define (mlhash-count mlh . args)
  (let outer-loop ([level (mlhash-levels mlh)]
                   [h (mlhash-root mlh)]
                   [args args])
    (if (not (null? args))
        ;; Not out of arguments, keep searching till we get to the root of the
        ;; sub-trees that we want to count for
        (let ([next-h (hash-ref h (first args) #f)])
          (if next-h
              (outer-loop (sub1 level) next-h (rest args))
              0))
        ;; Out of arguments - count recursively from this root
        (let inner-loop ([level level] [h h])
          (if (= 1 level)
              (hash-count h)
              (for/sum ([next-h (in-list (hash-values h))])
                (inner-loop (sub1 level) next-h)))))))

;; -- Tests --

(module+ test
  (require rackunit)

  ;; Verify that values can be set and retrieved.
  (test-case "Getting and Setting"
    (define mlh (make-mlhash 2))
    (set! mlh (mlhash-set mlh 0 0 'zero-zero))
    (set! mlh (mlhash-set mlh 0 1 'zero-one))
    (set! mlh (mlhash-set mlh 1 0 'one-zero))
    (set! mlh (mlhash-set mlh 1 1 'one-one))
    (check-equal? (mlhash-ref mlh 0 0) 'zero-zero)
    (check-equal? (mlhash-ref mlh 0 1) 'zero-one)
    (check-equal? (mlhash-ref mlh 1 0) 'one-zero)
    (check-equal? (mlhash-ref mlh 1 1) 'one-one))

  ;; Verify that the correct action occurs when a missing key is requested.
  (test-case "Missing Values"
    (define mlh (make-mlhash 2))
    (check-exn exn:fail? (thunk (mlhash-ref mlh 0 0)))
    (check-equal? (mlhash-ref mlh 0 0 #:failure-result 'missing) 'missing)
    (check-equal? (mlhash-ref mlh 0 0 #:failure-result (thunk "error")) "error"))

  ;; Verify that values can be removed from the mlhash.
  (test-case "Removing Values"
    (define mlh (make-mlhash 3))
    (set! mlh (mlhash-set mlh 0 0 0 'value1))
    (set! mlh (mlhash-set mlh 0 0 1 'value2))
    (set! mlh (mlhash-set mlh 0 1 0 'value3))
    (set! mlh (mlhash-set mlh 1 0 0 'value4))
    (check-equal?
     (mlhash-ref (mlhash-remove mlh 0 0 0) 0 0 0 #:failure-result 'missing)
     'missing)
    (check-equal?
     (mlhash-ref (mlhash-remove mlh 0 0) 0 0 1 #:failure-result 'missing)
     'missing)
    (check-equal?
     (mlhash-ref (mlhash-remove mlh 1) 1 0 0 #:failure-result 'missing)
     'missing)
    (check-equal?
     (mlhash-ref (mlhash-remove mlh) 0 0 0 #:failure-result 'missing)
     'missing)
    (check-equal?
     (mlhash-ref (mlhash-remove mlh 47) 0 0 0)
     'value1))

  ;; Verify that mlhash entry counting operates correctly.
  (test-case "Counts"
    (define mlh (make-mlhash 3))
    (set! mlh (mlhash-set mlh 0 0 0 'value1))
    (set! mlh (mlhash-set mlh 0 0 1 'value2))
    (set! mlh (mlhash-set mlh 0 1 0 'value3))
    (set! mlh (mlhash-set mlh 1 0 0 'value4))
    (check-equal? (mlhash-count mlh) 4)
    (check-equal? (mlhash-count mlh 0) 3)
    (check-equal? (mlhash-count mlh 0 0) 2)
    (check-equal? (mlhash-count mlh 0 1) 1)
    (check-equal? (mlhash-count mlh 1) 1)
    (check-equal? (mlhash-count mlh 1 0) 1)
    (check-equal? (mlhash-count mlh 1 1) 0)))
