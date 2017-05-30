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

;; -- Provides --

(provide
 (contract-out

  ;; Struct representing the command-line arguments to the program.
  [struct arguments ([worker-count exact-nonnegative-integer?]
                     [working-dir path-string?]
                     [interface (or/c string? false?)]
                     [port-number port-number?]
                     [client-timeout (or/c positive? false?)]
                     [minimum-log-event-level log-event-level?])]

  ;; Parses and validates arguments from the specified list of strings. Raises a
  ;; user error if the arguments are not in a valid state.
  [get-arguments (-> (listof string?) arguments?)]))

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

(define (get-arguments args-list)
  (let ([args (parse-arguments args-list)])
    (validate-arguments args)
    args))

;; -- Private Procedures --

;; Argument parsing procedure.
(define parse-arguments
  (arguments-parser
   arguments
   (arguments 1		; worker-count: default = 1
              #f	; working-dir: user must specify
              #f	; interface: default = #f (any)
              #f	; port-number: user must specify
              #f	; client-timeout: default = #f (none)
              'trace)	; log-level: default = trace
   (var ("-j" "--worker-count")
        worker-count
        (string->number-or-error (integer-in 1 64) "worker count"))
   (var ("-w" "--working-dir")
        working-dir)
   (var ("-i" "--interface")
        interface)
   (var ("-p" "--port")
        port-number
        (string->number-or-error port-number? "port number"))
   (var ("-z" "--client-timeout")
        client-timeout
        (string->number-or-error positive? "client timeout"))
   (var ("-l" "--log-level")
        minimum-log-event-level
        (string->log-event-level-or-error "log level"))))

;; Validates an arguments struct.
(define (validate-arguments args)
  ;; Validate working directory
  (let ([working-dir (arguments-working-dir args)])
    (when (not working-dir)
      (raise-user-error "Working directory not specified."))
    (when (not (directory-exists? working-dir))
      (raise-user-error (format "Invalid working directory: ~A" working-dir))))
  ;;; Validate port number
  (let ([port-number (arguments-port-number args)])
    (when (not port-number)
      (raise-user-error "Port number not specified."))))

;; Either converts a string to a number or raises a user error.
(define (string->number-or-error predicate argument)
  (λ (str)
    (let ([converted (string->number str)])
      (if (predicate converted)
          converted
          (raise-user-error (format "Invalid ~A: ~A" argument str))))))

;; Either converts a string to a log event level or raises a user error.
(define (string->log-event-level-or-error argument)
  (λ (str)
    (match str
      ["critical" 'critical]
      ["error" 'error]
      ["warning" 'warning]
      ["info" 'info]
      ["debug" 'debug]
      ["trace" 'trace]
      [else (raise-user-error (format "Invalid ~A: ~A" argument str))])))
