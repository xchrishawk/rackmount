;;
;; arguments.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures for handling command line arguments.
;;

#lang racket

;; -- Requires --

(require "../util/arguments-parser.rkt")
(require "../util/logging.rkt")
(require "../worker/client.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct representing the command-line arguments to the program.
  [struct arguments ([worker-count exact-nonnegative-integer?]
                     [working-dir path-string?]
                     [interface (or/c string? false?)]
                     [port-number port-number?]
                     [client-timeout client-timeout?]
                     [minimum-log-event-level log-event-level?])]

  ;; Parses and validates arguments from the specified list of strings. Raises a
  ;; user error if the arguments are not in a valid state.
  [parse-arguments (-> (listof string?) arguments?)]))

;; -- Types --

;; Struct representing the parsed command line arguments.
(struct arguments (worker-count
                   working-dir
                   interface
                   port-number
                   client-timeout
                   minimum-log-event-level)
  #:transparent)

;; -- Public Procedures --

(define parse-arguments
  (arguments-parser
   arguments
   (arguments 1		; worker-count: default = 1
              #f	; working-dir: user must specify
              #f	; interface: default = #f (any)
              #f	; port-number: user must specify
              #f	; client-timeout: default = #f (none)
              'trace)	; log-level: default = trace
   ([var ("-j" "--worker-count")
         worker-count
         #:proc string->number
         #:guard (integer-in 1 64)]
    [var ("-w" "--working-dir")
         working-dir
         #:guard (λ (value)
                   (and (path-string? value)
                        (directory-exists? value)))
         #:mandatory]
    [var ("-i" "--interface")
         interface]
    [var ("-p" "--port-number")
         port-number
         #:proc string->number
         #:guard port-number?
         #:mandatory]
    [var ("-z" "--client-timeout")
         client-timeout
         #:proc string->number
         #:guard (and/c real? positive?)]
    [var ("-l" "--log-level")
         minimum-log-event-level
         #:proc string->symbol
         #:guard log-event-level?])))
