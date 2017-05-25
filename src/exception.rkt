;;
;; exception.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require (for-syntax racket))
(require (for-syntax "utility.rkt"))

;; -- Macros --

(define-syntax (define-exn stx)
  (syntax-case stx ()
    [(_ exn-name)
     (let ([exn-name-string (symbol->string (syntax->datum #'exn-name))])
       (with-syntax ([exn-struct-name
                      (make-syntax stx "exn:fail:~A" exn-name-string)]
                     [extra-constructor-name
                      (make-syntax stx "make-exn:fail:~A" exn-name-string)]
                     [raise-proc-name
                      (make-syntax stx "raise-~A-error" (string-replace exn-name-string ":" "-"))])
         (syntax
          (begin
            (provide
             (contract-out
              [struct exn-struct-name ([message string?] [continuation-marks continuation-mark-set?])]
              [raise-proc-name (->* (string?) #:rest (listof any/c) any)]))
            (struct exn-struct-name exn:fail ()
              #:extra-constructor-name extra-constructor-name
              #:transparent)
            (define (raise-proc-name fmt . args)
              (raise (exn-struct-name (apply format fmt args)
                                      (current-continuation-marks))))))))]))

;; -- Definitions --

(define-exn rackmount)
(define-exn rackmount:invalid-request)
