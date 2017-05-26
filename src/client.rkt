;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This file defines the client thread, which is the primary thread responsible
;; for handling basic communication with a client after it has connected.
;;

#lang racket

;; -- Requires --

(require racket/date)
(require "exception.rkt")
(require "http-request.rkt")
(require "http-response.rkt")
(require "hypertext.rkt")
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
                     ;; Get the HTTP response to send, and return it to the client
                     (let*-values ([(response continue) (handle-request-string accumulated-request)]
                                   [(response-bytes) (http-response->bytes response)])
                       (write-bytes response-bytes output-port)
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

;; Handles a request string.
(define/contract (handle-request-string request-string)
  (-> string? (values http-response? boolean?))
  (let ([request (with-handlers ([exn:fail:rackmount:bad-http-request? (λ (ex) #f)])
                   (parse-http-request request-string))])
    (if request
        (handle-request request)
        (values (http-response-bad-request) #f))))

;; Handles a correctly parsed request.
(define/contract (handle-request request)
  (-> http-request? (values http-response? boolean?))
  (let* ([html-string (hypertext
                       (html
                        (head
                         (title "Echo Service"))
                        (body
                         (h1 "Echo Service")
                         (hr)
                         (h3 "Request Info")
                         (ul
                          (li (strong "Date") (text ": " (date->string (current-date) #t)))
                          (li (strong "Method") (text ": " (http-request-method request)))
                          (li (strong "URI") (text ": " (http-request-uri request)))
                          (li (strong "HTTP Major Version")
                              (text (format ": ~A" (http-request-version-major request))))
                          (li (strong "HTTP Minor Version")
                              (text (format ": ~A" (http-request-version-minor request)))))
                         (h3 "Headers")
                         (ul
                          (for ([(key value) (in-hash (http-request-headers request))])
                            (li (strong (text key))
                                (text ": ")
                                (text value)))))))]
         [html-bytes (string->bytes/utf-8 html-string)])
    (values (http-response-ok html-bytes) #f)))

;; Logs an event to the "Client" category.
(define client-log
  (create-local-log "Client"))
