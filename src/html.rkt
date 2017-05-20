#lang racket

;; -- Requires --

(require (for-syntax racket))

;; -- Provides --

(provide html)

;; -- Macros --

(define-syntax (html stx)
  (define (proc-clause clause)
    (let ([tag-name (symbol->string (first clause))])
      (list
       (format "<~A>" tag-name)
       (map (λ (subclause)
              (cond
                [(list? subclause) (proc-clause subclause)]
                [(string? subclause) subclause]
                [else (format "~A" subclause)]))
            (rest clause))
       (format "</~A>" tag-name))))
  (let ([result `(apply
                  string-append
                  (flatten
                   (quote
                    ,(proc-clause (syntax->datum stx)))))])
    (datum->syntax stx result)))
