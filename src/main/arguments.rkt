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
    [var ("-l" "--log-level")
         minimum-log-event-level
         #:proc string->symbol
         #:guard log-event-level?])))

;; -- Tests --

(module+ test

  ;; -- Requires --

  (require rackunit)

  ;; -- Test Cases --

  (test-case "Worker Count"
    (let ([args (parse-arguments '("-w" "." "-p" "8080"))])
      (check-equal? (arguments-worker-count args) 1))
    (let ([args (parse-arguments '("-w" "." "-p" "8080" "-j" "4"))])
      (check-equal? (arguments-worker-count args) 4))
    ;; Missing argument
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-j")))))

  (test-case "Working Directory"
    (let ([args (parse-arguments '("-w" "." "-p" "8080"))])
      (check-equal? (arguments-working-dir args) "."))
    ;; Working directory not specified
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-p" "8080"))))
    ;; Working directory not a valid path string
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "???" "-p" "8080"))))
    ;; Working directory does not exist
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "/doesnotexist" "-p" "8080"))))
    ;; Missing argument
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-p" "8080" "-w")))))

  (test-case "Interface"
    (let ([args (parse-arguments '("-w" "." "-p" "8080"))])
      (check-equal? (arguments-interface args) #f))
    (let ([args (parse-arguments '("-w" "." "-p" "8080" "-i" "interface"))])
      (check-equal? (arguments-interface args) "interface"))
    ;; Missing argument
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-i")))))

  (test-case "Port Number"
    (let ([args (parse-arguments '("-w" "." "-p" "8080"))])
      (check-equal? (arguments-port-number args) 8080))
    ;; Not a valid port number
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "-1"))))
    ;; Not an integer
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8.080"))))
    ;; Missing argument
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p")))))

  (test-case "Log Level"
    (let ([args (parse-arguments '("-w" "." "-p" "8080" "-l" "critical"))])
      (check-equal? (arguments-minimum-log-event-level args) 'critical))
    ;; Invalid log level
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-l" "notalevel"))))
    ;; Missing argument
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-l"))))))