;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require "utility.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Launches a client thread.
  [client-start
   (-> string?			; working-dir
       input-port?		; input-port
       output-port?		; output-port
       opaque-client?)]
  ;; Stops a client thread.
  [client-stop
   (-> opaque-client?		; client
       void?)]))

;; -- Types --

(struct opaque-client (thread))

;; -- Public Procedures --

(define (client-start working-dir input-port output-port)
  (let ([client-thread
         (thread-start
          (let loop ()
            ;; Wait for the next evenet
            (let ([evt (sync (read-line-evt input-port) (thread-receive-evt))])
              (cond
                ;; Received line from client
                [(string? evt)
                 (displayln evt)
                 (loop)]
                ;; Client disconnect
                [(equal? evt eof)
                 (void)]
                ;; Shutdown command
                [(and (equal? evt (thread-receive-evt))
                      (equal? (thread-receive) 'shutdown))
                 (void)]
                ;; Unknown event?
                [else (error "Unknown event??")])))
          ;; Clean up and exit thread
          (close-input-port input-port)
          (close-output-port output-port))])
    (opaque-client client-thread)))

(define (client-stop client)
  (let ([client-thread (opaque-client-thread client)])
    (thread-send client-thread 'shutdown)
    (void)))
