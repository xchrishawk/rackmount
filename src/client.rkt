;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require racket/date)
(require "html.rkt")
(require "http-request.rkt")
(require "log.rkt")
(require "utility.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Launches a client thread.
  [client-start (-> string? input-port? output-port? (-> any) opaque-client?)]
  ;; Terminates a client thread.
  [client-stop (-> opaque-client? void?)]))

;; -- Types --

(struct opaque-client (thread))

;; -- Public Procedures --

(define (client-start working-dir input-port output-port shutdown-closure)
  (let ([client-thread
         (thread-start
          (client-log "Client connected. Client thread starting...")
          (let loop ([accumulated-request (string)])
            ;; Wait for the next event
            (let ([evt (sync (read-line-evt input-port 'any)
                             (thread-receive-evt))])
              (cond
                ;; Received line from client
                [(string? evt)
                 (if (string-empty? evt)
                     ;; Blank line - this request is complete
                     (let-values ([(response continue) (handle-request accumulated-request)])
                       (display response output-port)
                       (when continue
                         (loop (string))))
                     ;; Non-blank line - continue accumulating
                     (loop (string-append accumulated-request "\n" evt)))]
                ;; Client disconnected from their end
                [(equal? evt eof)
                 (client-log "Client disconnected.")
                 (void)]
                ;; Shutdown command
                [(and (equal? evt (thread-receive-evt))
                      (equal? (thread-receive) 'shutdown))
                   (void)]
                ;; Unknown event?
                [else (error "Unknown event??")])))
          ;; Shut down ports
          (close-input-port input-port)
          (close-output-port output-port)
          ;; Perform shutdown closure then end thread
          (shutdown-closure)
          (client-log "Client thread terminated normally."))])
    ;; Return an opaque object representing the client
    (opaque-client client-thread)))

(define (client-stop client)
  (let ([client-thread (opaque-client-thread client)])
    ;; There is a potential that the client thread may have already shut itself down
    ;; if the client disconnected, so tolerate that case.
    (when (thread-running? client-thread)
      (thread-send client-thread 'shutdown)
      (sync client-thread)
      (void))))

;; -- Private Procedures --

(define (handle-request request-string)
  (let ([request (parse-http-request request-string)])
    (values
     (string-append "HTTP/1.0 200 OK\n\n"
                    (html
                     (head
                      (title "Echo Service"))
                     (body
                      (h1 "Echo Service")
                      (hr)
                      (ul
                       (li (strong "Date") ": " (date->string (current-date) #t))
                       (li (strong "Method") ": " (http-request-method request))
                       (li (strong "URI") ": " (http-request-uri request))
                       (li (strong "HTTP Major Version")
                           (format ": ~A" (http-request-version-major request)))
                       (li (strong "HTTP Minor Version")
                           (format ": ~A" (http-request-version-minor request)))))))
     #f)))

(define client-log (create-local-log "Client"))
