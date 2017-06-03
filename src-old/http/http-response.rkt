;;
;; http-response.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines a primitive type representing an HTTP response, as well
;; as some basic operations on that type and helper functions to generate simple
;; responses.
;;

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; Converts an HTTP response to a formatted response string.
  [http-response->bytes (-> http-response? bytes?)]

  ;; Generates a 200 OK response.
  [http-response-ok (->* (bytes?) (headers?) http-response?)]

  ;; Generates a 200 OK response.
  [http-response-ok/utf-8 (->* (string?) (headers?) http-response?)]

  ;; Generates a 400 Bad Request response.
  [http-response-bad-request (->* () (headers?) http-response?)]

  ;; Struct representing an HTTP response.
  [struct http-response ([status-code (integer-in 100 599)]
                         [reason string?]
                         [version-major exact-positive-integer?]
                         [version-minor exact-positive-integer?]
                         [headers headers?]
                         [content bytes?])]))

;; -- Structs --

(struct http-response (status-code
                       reason
                       version-major
                       version-minor
                       headers
                       content)
  #:transparent)

;; -- Public Procedures --

(define (http-response->bytes response)
  (let ([response-bytes (open-output-bytes)])
    ;; Status line
    (let* ([status-line-string (format "HTTP/~A.~A ~A ~A\r\n"
                                       (http-response-version-major response)
                                       (http-response-version-minor response)
                                       (http-response-status-code response)
                                       (http-response-reason response))]
           [status-line-bytes (string->bytes/utf-8 status-line-string)])
      (write-bytes status-line-bytes response-bytes))
    ;; Headers
    (when (http-response-headers response)
      (for ([(key value) (in-hash (http-response-headers response))])
        (let* ([header-string (format "~A: ~A\r\n" key value)]
               [header-bytes (string->bytes/utf-8 header-string)])
          (write-bytes header-bytes response-bytes))))
    ;; Content
    (write-bytes #"\r\n" response-bytes)
    (write-bytes (http-response-content response) response-bytes)
    ;; Return the final byte strng
    (get-output-bytes response-bytes)))

(define (http-response-ok content [headers #f])
  (http-response 200 "OK" 1 1
                 (hash-set* (or headers (hash))
                            "Content-Length" (number->string (bytes-length content))
                            "Server" "Rackmount")
                 content))

(define (http-response-ok/utf-8 content-string [headers #f])
  (http-response-ok (string->bytes/utf-8 content-string) headers))

(define (http-response-bad-request [headers #f])
  (http-response 400 "Bad Request" 1 1 headers #""))

;; -- Private Procedures --

;; Predicate returning #t if the argument is a valid headers feld.
(define (headers? x)
  ((or/c (hash/c string? string?) false?) x))
