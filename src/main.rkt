;;
;; main.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Main entry point for the program.
;;

#lang racket

(define (main args-list)
  (void))

(module+ main
  (let ([args-list (vector->list (current-command-line-arguments))])
    (main args-list)))
