;;
;; server.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require "client.rkt")
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
  (let ([listener
         (listener-start
          hostname
          port
          #t	; reusable
          4	; allow max 4 clients waiting
          (λ (input-port output-port)
            (let ([client (client-start working-dir input-port output-port)])
              (void))))])
    (opaque-server listener)))

(define (server-stop server)
  (listener-stop (opaque-server-listener server)))
