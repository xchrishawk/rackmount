;;
;; define-thread.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Contains macros and procedures for setting up one of the application's main
;; threads.
;;

#lang racket

;; -- Requires --

(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))
(require (for-syntax "../util/syntax.rkt"))

;; -- Provides --

(provide

 ;; Defines a main place thread.
 define-thread)

;; -- Macros --

(define-syntax (define-thread stx)
  (syntax-parse stx
    [(_ thread-name:id config-struct:id main-proc:id)
     (let ([thread-name-string (symbol->string (syntax->datum #'thread-name))]
           [config-struct-name (symbol->string (syntax->datum #'config-struct))])
       (with-syntax ([thread-struct
                      (make-syntax/symbol stx "opaque-~A" thread-name-string)]
                     [thread-struct-thread
                      (make-syntax/symbol stx "opaque-~A-thread" thread-name-string)]
                     [thread-struct-pred
                      (make-syntax/symbol stx "opaque-~A?" thread-name-string)]
                     [config-struct-pred
                      (make-syntax/symbol stx "~A?" config-struct-name)]
                     [start-proc
                      (make-syntax/symbol stx "~A-start" thread-name-string)]
                     [stop-proc
                      (make-syntax/symbol stx "~A-stop" thread-name-string)]
                     [config-arg
                      (generate-temporary "config")]
                     [thread-arg
                      (generate-temporary "thread")]
                     [thread-temp
                      (generate-temporary "thd")])
         (let ([result
                `(begin
                   (provide
                    (contract-out
                     [,#'start-proc (-> ,#'config-struct-pred ,#'thread-struct-pred)]
                     [,#'stop-proc (-> ,#'thread-struct-pred void?)]))
                   (struct ,#'thread-struct (thread))
                   (define (,#'start-proc ,#'config-arg)
                     (,#'thread-struct (thread (λ () (,#'main-proc ,#'config-arg)))))
                   (define (,#'stop-proc ,#'thread-arg)
                     (let ([,#'thread-temp (,#'thread-struct-thread ,#'thread-arg)])
                       (thread-send ,#'thread-temp 'shutdown)
                       (sync ,#'thread-temp))
                     (void)))])
           (datum->syntax stx result))))]))
