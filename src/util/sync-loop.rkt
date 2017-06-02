;;
;; sync-loop.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require (for-syntax racket))
(require (for-syntax syntax/parse))

;; -- Provides --

(provide sync-loop)

;; -- Macros --

(define-syntax (sync-loop stx)

  (define-syntax-class arg-spec
    #:description "loop argument"
    (pattern (id:id default:expr)))

  (define-syntax-class evt-spec
    #:description "event spec"
    (pattern (id:expr proc:id)))

  (syntax-parse stx
    [(_ (arg-specs:arg-spec ...) (evt-specs:evt-spec ...+) b ...)
     (let* ([arg-specs (syntax->datum (syntax (arg-specs ...)))]
            [arg-names (map first arg-specs)]
            [evt-specs (syntax->datum (syntax (evt-specs ...)))]
            [evt-clauses (map (λ (evt-spec)
                                `(wrap-evt ,(first evt-spec)
                                           (λ (result)
                                             (,(second evt-spec) ,@arg-names result))))
                              evt-specs)]
            [inner-loop `(let loop (,@arg-specs)
                                     (let-values ([(,@arg-names continue) (sync ,@evt-clauses)])
                                       (if continue
                                           (loop ,@arg-names)
                                           (values ,@arg-names))))]
            [result
             (cond
               [(null? (syntax-e #'(b ...))) inner-loop]
               [(null? arg-specs) `(begin
                                     ,inner-loop
                                     ,@#'(b ...))]
               [else `(let-values ([(,@arg-names) ,inner-loop])
                        ,@#'(b ...))])])
       (datum->syntax stx result))]))
