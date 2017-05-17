#lang racket

;; -- Requires --

(require (for-syntax racket))
(require (for-syntax racket/syntax))

;; -- Provides --

(provide arg-parser)

;; -- Macros --

(define-syntax (arg-parser stx)
  (let* ([syntax-datum (syntax->datum stx)]
         [struct-id (second syntax-datum)]
         [struct-expr (third syntax-datum)]
         [clauses (drop syntax-datum 3)]
         [args (syntax->datum (generate-temporary "args"))]
         [result (syntax->datum (generate-temporary "result"))])
    (datum->syntax
     stx
     `(λ (,args)
        ;; Recursive loop through all of the arguments
        (let loop ([,args ,args] [,result ,struct-expr])
          (if (null? ,args)
              ,result
              (let ([arg (first ,args)])
                (cond
                  ;; Add a cond case for each clause
                  ,@(map
                     (λ (clause)
                       ;; Helper function for "var" clauses
                       (define (var-clause predicate fld-id proc)
                         `[,predicate
                           (if (not (null? (rest ,args)))
                               (loop
                                (drop ,args 2)
                                (struct-copy ,struct-id ,result [,fld-id (,proc (second ,args))]))
                               (raise-user-error (format "Missing argument for ~A" arg)))])
                       ;; Helper function for "flag" clauses
                       (define (flag-clause predicate fld-id)
                         `[,predicate
                           (loop
                            (rest ,args)
                            (struct-copy ,struct-id ,result [,fld-id #t]))])
                       ;; Match the specific clause type that we got
                       (match clause
                         ;; Unprocessed string, allow multiple argument names
                         [(list 'var (list arg-name ...) fld-id)
                          (var-clause `(member arg (list ,@arg-name)) fld-id identity)]
                         ;; Unprocessed string, single argument name only
                         [(list 'var arg-name fld-id)
                          (var-clause `(equal? arg ,arg-name) fld-id identity)]
                         ;; Processed/converted value, allow multiple argument names
                         [(list 'var (list arg-name ...) fld-id proc)
                          (var-clause `(member arg (list ,@arg-name)) fld-id proc)]
                         ;; Processed/converted value, single argument name only
                         [(list 'var arg-name fld-id proc)
                          (var-clause `(equal? arg ,arg-name) fld-id proc)]
                         ;; Flag value, allow multiple argument names
                         [(list 'flag (list arg-name ...) fld-id)
                          (flag-clause `(member arg (list ,@arg-name)) fld-id)]
                         ;; Flag value, single argument name only
                         [(list 'flag arg-name fld-id)
                          (flag-clause `(equal? arg ,arg-name) fld-id)]))
                     clauses)
                  [else (raise-user-error (format "Unknown argument: ~A" arg))]))))))))

;; -- Example --

(define (example args-list)
  (struct example-args (a b c) #:transparent)
  (define parse (arg-parser example-args
                            (example-args #f #f #f)
                            (var ("-a" "--longnamefora") a)
                            (var "-b" b string->number)
                            (flag ("-c" "--longnameforc") c)))
  (println (parse args-list)))

(module+ main
  (example (vector->list (current-command-line-arguments))))
