#!/usr/bin/racket
#lang racket

;; -- Requires --

(require "arg-parser.rkt")
(require "log.rkt")
(require "server.rkt")

;; -- Types --

(struct arguments (working-dir hostname port) #:transparent)

;; -- Main Procedure --

(define (main args-list)
  (let ([args (parse-arguments args-list)])
    (main-log "Launched with arguments \"~A\"" (string-join args-list " "))
    (validate-arguments args)
    (let* ([config (server-config (arguments-working-dir args)
                                  (arguments-hostname args)
                                  (arguments-port args)
                                  #t
                                  4)]
           [server (server-start config)])
      (wait-for-break)
      (main-log "Received break, terminating server...")
      (server-stop server))))

;; -- Private Procedures --

;; Parses an argument list.
(define parse-arguments
  (arg-parser
   arguments
   (arguments #f #f #f)
   (var ("-w" "--working-dir") working-dir)
   (var ("-h" "--hostname") hostname)
   (var ("-p" "--port") port
        (λ (x)
          (let ([converted (string->number x)])
            (or converted (raise-user-error (format "Invalid port: ~A" x))))))))

;; Validates an arguments struct.
(define (validate-arguments args)
  ;; Working directory
  (let ([working-dir (arguments-working-dir args)])
    (when (not working-dir)
      (raise-user-error "Working directory not specified"))
    (when (not (directory-exists? working-dir))
      (raise-user-error (format "Working directory does not exist: ~A" working-dir))))
  ;; Port
  (let ([port (arguments-port args)])
    (when (not port)
      (raise-user-error "Port not specified"))
    (when (not (and (exact-positive-integer? port)
                    (<= 1 port 65535)))
      (raise-user-error (format "Invalid port: ~A" port)))))

;; Blocks until receiving a break.
(define (wait-for-break)
  (with-handlers ([exn:break? void])
    (let ([sem (make-semaphore)])
      (semaphore-wait sem))))

;; Logs an event to the "Main" category.
(define main-log (create-local-log "Main"))

;; -- Main Module --

(module+ main
  (main (vector->list (current-command-line-arguments))))
