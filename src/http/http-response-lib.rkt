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
       (#:extra-headers (maybe/c (hash/c string? string?))
        #:entity (maybe/c bytes?)
        #:http-major-version exact-nonnegative-integer?
        #:http-minor-version exact-nonnegative-integer?)
       http-response?))

(provide
 (contract-out

  ;; Generates a 200 OK response.
  [http-response-ok response-contract]

  ;; Generates a 400 Bad Request response.
  [http-response-bad-request response-contract]

  ;; Generates a 500 Internal Server Error response.
  [http-response-internal-server-error response-contract]))

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
     (let ([default-headers (default-headers)])
       (if extra-headers
           (hash-union default-headers extra-headers)
           default-headers))
     ;; Entity bodye
     entity)))

;; Generates the default headers which should be included with every response.
(define (default-headers)
  (hash
   ;; Server header (RFC 2616 section 14.38)
   "Server" (server-name)))

;; -- Response Library --

(define http-response-ok
  (response 200))

(define http-response-bad-request
  (response 400))

(define http-response-internal-server-error
  (response 500))
