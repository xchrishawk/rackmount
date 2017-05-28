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

(struct arguments (worker-count		; number of worker places to launch
                   working-dir		; working directory for server
                   interface		; host name of interface to bind to
                   port-number		; port number to bind to
                   client-timeout)	; max time to wait for data from client
  #:transparent)

;; -- Main Procedure --

(define (main args-list)
  (let ([args (parse-arguments args-list)])
    (main-log "Launched with arguments \"~A\"" (string-join args-list " "))
    (validate-arguments args)
    (let ([config (server-config (arguments-worker-count args)
                                 (arguments-working-dir args)
                                 (arguments-interface args)
                                 (arguments-port-number args)
                                 (let ([timeout (arguments-client-timeout args)])
                                   (if timeout (* timeout 1000.0) #f))
                                 #t	; reusable
                                 4)])	; max wait
      (server-run config))))

;; -- Private Procedures --

;; Parses an argument list.
(define parse-arguments
  (arg-parser
   arguments
   (arguments 1		; worker-count: default = 1
              #f	; working-dir: user must set
              #f	; interface: default = #f (any)
              #f	; port-number: user must set
              #f)	; client-timeout: default = #f (no timeout)
   (var ("-j" "--workers")
        worker-count
        (string->number-or-error (integer-in 1 64) "job count"))
   (var ("-w" "--working-dir")
        working-dir)
   (var ("-i" "--interface")
        interface)
   (var ("-p" "--port")
        port-number
        (string->number-or-error port-number? "port number"))
   (var ("-z" "--client-timeout")
        client-timeout
        (string->number-or-error positive? "client timeout"))))

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
