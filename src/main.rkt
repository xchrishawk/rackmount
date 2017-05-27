;;
;; main.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require "arg-parser.rkt")
(require "log.rkt")
(require "server.rkt")

;; -- Types --

(struct arguments (worker-count
                   max-thread-count
                   working-dir
                   interface
                   port-number)
  #:transparent)

;; -- Main Procedure --

(define (main args-list)
  (let ([args (parse-arguments args-list)])
    (main-log "Launched with arguments \"~A\"" (string-join args-list " "))
    (validate-arguments args)
    (let ([config (server-config (arguments-worker-count args)
                                 (arguments-max-thread-count args)
                                 (arguments-working-dir args)
                                 (arguments-interface args)
                                 (arguments-port-number args)
                                 #t	; reusable
                                 4)])	; max wait
      (server-run config))))

;; -- Private Procedures --

;; Parses an argument list.
(define parse-arguments
  (arg-parser
   arguments
   (arguments 1		; worker-count: default = 1
              1		; max-thread-count: default = 1
              #f	; working dir: user must set
              #f	; interface: default = #f (any)
              #f)	; port number: user must set
   (var ("-j" "--workers")
        worker-count
        (string->number-or-error (integer-in 1 64) "job count"))
   (var ("-t" "--max-threads")
        max-thread-count
        (string->number-or-error (integer-in 1 64) "max thread count"))
   (var ("-w" "--working-dir")
        working-dir)
   (var ("-i" "--interface")
        interface)
   (var ("-p" "--port")
        port-number
        (string->number-or-error port-number? "port number"))))

;; Either converts a string to a number or raises a user error.
(define (string->number-or-error predicate argument)
  (λ (str)
    (let ([converted (string->number str)])
      (if (predicate converted)
          converted
          (raise-user-error (format "Invalid ~A: ~A" argument str))))))

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

;; Logs an event to the "Main" category.
(define main-log
  (create-local-log "Main"))

;; -- Main Module --

(module+ main
  (main (vector->list (current-command-line-arguments))))
