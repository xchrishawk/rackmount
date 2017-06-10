;;
;; arguments.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures for handling command line arguments.
;;

#lang racket

;; -- Requires --

(require "../server/session.rkt")
(require "../util/arguments-parser.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct representing the command-line arguments to the program.
  [struct arguments ([worker-count exact-nonnegative-integer?]
                     [working-dir path-string?]
                     [interface (maybe/c string?)]
                     [port-number port-number?]
                     [session-timeout session-timeout?]
                     [minimum-log-event-level log-event-level?]
                     [override-server-name (maybe/c string?)])]

  ;; Parses and validates arguments from the specified list of strings. Raises a
  ;; user error if the arguments are not in a valid state.
  [parse-arguments (-> (listof string?) arguments?)]))

;; -- Types --

;; Struct representing the parsed command line arguments.
(struct arguments (worker-count
                   working-dir
                   interface
                   port-number
                   session-timeout
                   minimum-log-event-level
                   override-server-name)
  #:transparent)

;; -- Public Procedures --

(define parse-arguments
  (arguments-parser
   arguments
   (arguments 1		; worker-count: default = 1
              #f	; working-dir: user must specify
              #f	; interface: default = #f (any)
              #f	; port-number: user must specify
              #f	; session-timeout: default = #f (none)
              'trace	; log-level: default = trace
              #f)	; override-server-name: default = #f (none)
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
    [var ("-z" "--session-timeout")
         session-timeout
         #:proc string->number
         #:guard (and/c real? positive?)]
    [var ("-l" "--log-level")
         minimum-log-event-level
         #:proc string->symbol
         #:guard log-event-level?]
    [var ("-s" "--server-name")
         override-server-name])))

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

  (test-case "Session Timeout"
    (let ([args (parse-arguments '("-w" "." "-p" "8080"))])
      (check-equal? (arguments-session-timeout args) #f))
    (let ([args (parse-arguments '("-w" "." "-p" "8080" "-z" "9.5"))])
      (check-equal? (arguments-session-timeout args) 9.5))
    ;; Invalid value
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-z" "x14.x"))))
    ;; Negative timeout
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-z" "-15"))))
    ;; Missing argument
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-z")))))

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
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-l")))))

  (test-case "Override Server Name"
    (let ([args (parse-arguments '("-w" "." "-p" "8080"))])
      (check-equal? (arguments-override-server-name args) #f))
    (let ([args (parse-arguments '("-w" "." "-p" "8080" "-s" "override-server-name"))])
      (check-equal? (arguments-override-server-name args) "override-server-name"))
    ;; Missing argument
    (check-exn
     exn:fail:user?
     (thunk (parse-arguments '("-w" "." "-p" "8080" "-s"))))))
