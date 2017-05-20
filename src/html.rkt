#lang racket

;; -- Requires --

(require (for-syntax racket))
(require (for-syntax "utility.rkt"))

;; -- Provides --

(provide html)

;; -- Macros --

(define-syntax (html stx)
  ;; Helper - separates attributes from "normal" contents
  (define (proc-attributes unwrapped)
    (let-values ([(unused attributes)
                  (let loop ([remaining unwrapped] [unused null] [attributes (hash)])
                    (if (null? remaining)
                        (values unused attributes)
                        (let* ([this-stx (first remaining)]
                               [this-datum (syntax->datum this-stx)])
                          (if (and (length-at-least remaining 2)
                                   (keyword? this-datum))
                              ;; is a keyword
                              (loop (drop remaining 2)
                                    unused
                                    (hash-set attributes
                                              (keyword->string this-datum)
                                              (second remaining)))
                              ;; is not a keyword
                              (loop (rest remaining)
                                    (cons this-stx unused)
                                    attributes)))))])
      (values (reverse unused) attributes)))
  ;; Helper - processes a single clause
  (define (proc-clause clause)
    (let ([unwrapped (syntax-e clause)])
      (if (list? unwrapped)
          ;; This is a new clause to process
          (let*-values ([(contents attributes) (proc-attributes unwrapped)])
            (let ([tag-name (symbol->string (syntax->datum (first contents)))])
              `("<"
                ,tag-name
                ,@(hash-map attributes (λ (name value) (list (format " ~A=\"" name) value "\"")))
                ">"
                ,@(map proc-clause (rest contents))
                "</"
                ,tag-name
                ">")))
          ;; Something else, probably a literal - return unmodified
          unwrapped)))
  (let ([result `(string-append ,@(flatten (proc-clause stx)))])
    (datum->syntax stx result)))
