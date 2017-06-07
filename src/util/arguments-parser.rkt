;;
;; arguments-parser.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Defines a macro used to parse out a list of arguments into a struct.
;;

#lang racket

;; -- Requires --

(require (for-syntax racket))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;; -- Provides --

(provide arguments-parser)

;; -- Macros --

(define-syntax (arguments-parser stx)

  (define-syntax-class struct-id
    #:description "struct id"
    (pattern id))

  (define-syntax-class default-struct
    #:description "default arguments struct expression"
    (pattern expr))

  (define-syntax-class arg-names
    #:description "argument names"
    (pattern (names:str ...)))

  (define-syntax-class flag-spec
    #:description "flag specification"
    #:datum-literals (flag)
    (pattern ((~datum flag)
              arg-names:arg-names
              field-id:id)))

  (define-syntax-class var-spec
    #:description "variable specification"
    #:datum-literals (var)
    (pattern ((~datum var)
              arg-names:arg-names
              field-id:id
              (~or (~optional (~seq #:proc maybe-proc:expr))
                   (~optional (~seq #:guard maybe-guard:expr))
                   (~optional (~seq (~and #:mandatory mandatory-kw))))
              ...)
             #:with proc
             (if (attribute maybe-proc) #'maybe-proc #'identity)
             #:with guard
             (if (attribute maybe-guard) #'maybe-guard #'(λ (x) #t))
             #:with mandatory
             (if (attribute mandatory-kw) #t #f)))

  (syntax-parse stx
    [(_ struct-id:struct-id default:expr ((~or flag:flag-spec var:var-spec) ...))
     (let ([expanded
            #`(λ (argument-list)
                ;; Accumulate the result struct
                (let-values
                    ([(result used-args)
                      (let loop ([argument-list argument-list] [result default] [used-args (set)])
                        (if (null? argument-list)
                            (values result used-args)
                            (let*-values
                                ([(argument-name) (first argument-list)]
                                 [(argument-list) (rest argument-list)]
                                 [(argument-list result)
                                  (match argument-name
                                    #,@(map (λ (names field-id)
                                              #`[(or #,@names)
                                                 (values
                                                  argument-list
                                                  (struct-copy struct-id result (#,field-id #t)))])
                                            (syntax-e #'(flag.arg-names ...))
                                            (syntax-e #'(flag.field-id ...)))
                                    #,@(map (λ (names field-id proc guard)
                                              #`[(or #,@names)
                                                 (when (null? argument-list)
                                                   (raise-user-error
                                                    (format "Argument missing value: ~A" argument-name)))
                                                 (let* ([value (first argument-list)]
                                                        [processed-value (#,proc value)])
                                                   (when (not (#,guard processed-value))
                                                     (raise-user-error
                                                      (format "Invalid argument for ~A: ~A"
                                                              argument-name
                                                              value)))
                                                   (values
                                                    (rest argument-list)
                                                    (struct-copy struct-id
                                                                 result
                                                                 (#,field-id processed-value))))])
                                            (syntax-e #'(var.arg-names ...))
                                            (syntax-e #'(var.field-id ...))
                                            (syntax-e #'(var.proc ...))
                                            (syntax-e #'(var.guard ...)))
                                    [else
                                     (raise-user-error
                                      (format "Unrecognized argument: ~A" argument-name))])])
                              (loop argument-list result (set-add used-args argument-name)))))])
                  ;; Verify all mandatory arguments are accounted for
                  (let ([mandatory-argument-names
                         (list
                          #,@(foldl (λ (names mandatory result)
                                      (if mandatory
                                          (cons `(list ,@names) result)
                                          result))
                                    null
                                    (syntax->datum #'(var.arg-names ...))
                                    (syntax->datum #'(var.mandatory ...))))])
                    (for ([argument-names (in-list mandatory-argument-names)])
                      (when (not (ormap (λ (argument-name)
                                          (set-member? used-args argument-name))
                                        argument-names))
                        (raise-user-error (format "Mandatory argument missing: ~A"
                                                  (string-join argument-names " / "))))))
                  ;; Return the processed struct
                  result))])
       expanded)]))
