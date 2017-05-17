#!/usr/bin/racket
#lang racket

;; -- Requires --

(require "arg-parser.rkt")

;; -- Types --

(struct args (working-dir hostname port) #:transparent)

;; -- Main Procedure --

(define (main args-list)
  (let ([parsed-args (parse-args args-list)])
    (displayln parsed-args)))

;; -- Private Procedures --

;; Parses an argument list
(define parse-args
  (arg-parser
   args
   (args #f #f #f)
   (var ("-w" "--working-dir") working-dir)
   (var ("-h" "--hostname") hostname)
   (var ("-p" "--port") port
        (λ (x)
          (let ([converted (string->number x)])
            (or converted (raise-user-error (format "Invalid port: ~A" x))))))))

;; -- Main Module --

(module+ main
  (main (vector->list (current-command-line-arguments))))
