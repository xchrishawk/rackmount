;;
;; http-response-lib.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Module defining standard HTTP responses.
;;

#lang racket

;; -- Requires --

(require racket/hash)
(require (for-syntax syntax/parse))
(require "../http/http-response.rkt")
(require "../main/configuration.rkt")
(require "../util/misc.rkt")

;; -- Provides --

;; Contract for HTTP response-generating procedures.
(define response-contract
  (->* ()
       (#:extra-headers (maybe/c (hash/c string? any/c))
        #:entity (maybe/c (or/c bytes? input-port?))
        #:http-major-version exact-nonnegative-integer?
        #:http-minor-version exact-nonnegative-integer?)
       http-response?))

(provide
 (contract-out

  ;; Generates a 200 OK response.
  [http-response-ok response-contract]

  ;; Generates a 400 Bad Request response.
  [http-response-bad-request response-contract]

  ;; Generates a 404 Not Found response.
  [http-response-not-found response-contract]

  ;; Generates a 500 Internal Server Error response.
  [http-response-internal-server-error response-contract]

  ;; Generates a 501 Not Implemented response.
  [http-response-not-implemented response-contract]))

;; -- Constants --

(define default-http-major-version 1)
(define default-http-minor-version 1)

;; -- Private Procedures --

;; Builds a procedure to generate a specific response.
(define (response status-code #:custom-reason [custom-reason #f])
  (λ (#:extra-headers [extra-headers #f]
      #:entity [entity #f]
      #:http-major-version [http-major-version default-http-major-version]
      #:http-minor-version [http-minor-version default-http-minor-version])
    (http-response
     ;; Status code
     status-code
     ;; Reason
     (if custom-reason
         custom-reason
         (http-response-standard-reason status-code))
     ;; HTTP version
     http-major-version
     http-minor-version
     ;; Headers
     (let ([default-headers (default-headers entity)])
       (if extra-headers
           (hash-union default-headers extra-headers)
           default-headers))
     ;; Entity body
     entity)))

;; Generates the default headers which should be included with every response.
(define (default-headers entity)
  (let ([result (hash)])
    (define (set-header header-name header-value)
      (set! result (hash-set result header-name header-value)))
    ;; Server header (RFC 2616 section 14.38)
    (set-header "Server" (config-server-name))
    ;; Content-Length header (RFC 2616 section 14.13)
    (set-header "Content-Length" (entity-content-length entity))
    result))

(define (entity-content-length entity)
  (match entity
    ;; Entity is an input port
    [input-port?
     (let ([current-position (file-position entity)])
       (file-position entity eof)
       (begin0
           (file-position entity)
         (file-position entity current-position)))]
    ;; Entity is a byte string
    [bytes? (bytes-length entity)]
    ;; No entity
    [else 0]))

;; -- Response Library --

(define http-response-ok
  (response 200))

(define http-response-bad-request
  (response 400))

(define http-response-not-found
  (response 404))

(define http-response-internal-server-error
  (response 500))

(define http-response-not-implemented
  (response 501))
