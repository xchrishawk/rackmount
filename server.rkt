#lang racket

;; -- Requires --

(require "listener.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Launches a server instance.
  [server-start
   (-> string?			; working-dir
       (or/c string? false?)	; hostname
       (integer-in 1 65535)	; port
       opaque-server?)]
  ;; Terminates a server instance.
  [server-stop
   (-> opaque-server?		; server
       void?)]))

;; -- Types --

(struct opaque-server (listener))

;; -- Public Procedures --

(define (server-start working-dir hostname port)
  (let ([listener (listener-start hostname
                                  port
                                  #t
                                  4
                                  (λ (input-port output-port)
                                    (close-input-port input-port)
                                    (close-output-port output-port)))])
    (opaque-server listener)))

(define (server-stop server)
  (listener-stop (opaque-server-listener server)))
