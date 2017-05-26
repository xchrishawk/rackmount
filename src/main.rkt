#!/usr/bin/racket
#lang racket

;; -- Requires --

(require "arg-parser.rkt")
(require "log.rkt")
(require "server.rkt")

;; -- Types --

(struct arguments (worker-count
                   max-thread-count)
  #:transparent)

;; -- Main Procedure --

(define (main args-list)
  (let ([args (parse-arguments args-list)])
    (main-log "Launched with arguments \"~A\"" (string-join args-list " "))
    (validate-arguments args)
    (let ([config (server-config (arguments-worker-count args)
                                 (arguments-max-thread-count args))])
      (server-run config))))

;; -- Private Procedures --

;; Parses an argument list.
(define parse-arguments
  (arg-parser
   arguments
   (arguments 1		; worker-count: default = 1
              1)	; max-thread-count: default = 1
   (var ("-j" "--workers") worker-count
        (λ (x) (string->number-or-error x (integer-in 1 64) "job count")))
   (var ("-t" "--max-threads") max-thread-count
        (λ (x) (string->number-or-error x (integer-in 1 64) "max thread count")))))

;; Either converts a string to a number or raises a user error.
(define (string->number-or-error str predicate argument)
  (let ([converted (string->number str)])
    (if (predicate converted)
        converted
        (raise-user-error (format "Invalid ~A: ~A" argument str)))))

;; Validates an arguments struct.
(define (validate-arguments args)
  (void))

;; Logs an event to the "Main" category.
(define main-log
  (create-local-log "Main"))

;; -- Main Module --

(module+ main
  (main (vector->list (current-command-line-arguments))))
