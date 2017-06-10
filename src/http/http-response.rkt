;;
;; http-response.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures for generating HTTP responses.
;;

#lang racket

;; -- Requires --

(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct representing an HTTP response.
  [struct http-response ([status-code http-status-code?]
                         [reason string?]
                         [major-version exact-nonnegative-integer?]
                         [minor-version exact-nonnegative-integer?]
                         [headers (hash/c string? string?)]
                         [entity (maybe/c bytes?)])]

  ;; Predicate returning #t if the argument is a valid HTTP response code.
  [http-status-code? (-> any/c boolean?)]

  ;; Returns the standard reason string for the specified status code, or #f if no
  ;; matching standard reason string was found.
  [http-response-standard-reason (-> http-status-code? (maybe/c string?))]

  ;; Serializes an HTTP response to a byte string.
  [http-response->bytes (-> http-response? bytes?)]))

;; -- Structs --

(struct http-response (status-code
                       reason
                       major-version
                       minor-version
                       headers
                       entity)
  #:transparent)

;; -- Objects --

(define reason-lookup
  ;; RFC 2612 section 6.1.1
  (hash 100 "Continue"
        101 "Switching Protocols"
        200 "OK"
        201 "Created"
        202 "Accepted"
        203 "Non-Authoritative Information"
        204 "No Content"
        205 "Reset Content"
        206 "Partial Content"
        300 "Multiple Choices"
        301 "Moved Permanently"
        302 "Found"
        303 "See Other"
        304 "Not Modified"
        305 "Use Proxy"
        307 "Temporary Redirect"
        400 "Bad Request"
        401 "Unauthorized"
        402 "Payment Required"
        403 "Forbidden"
        404 "Not Found"
        405 "Method Not Allowed"
        406 "Not Acceptable"
        407 "Proxy Authentication Required"
        408 "Request Time-out"
        409 "Conflict"
        410 "Gone"
        411 "Length Required"
        412 "Precondition Failed"
        413 "Request Entity Too Large"
        414 "Request URI Too Large"
        415 "Unsupported Media Type"
        416 "Requested Range Not Satisfiable"
        417 "Expectation Failed"
        500 "Internal Server Error"
        501 "Not Implemented"
        502 "Bad Gateway"
        503 "Service Unavailable"
        504 "Gateway Timeout"
        505 "HTTP Version Not Supported"))

;; -- Public Procedures --

(define http-status-code?
  ;; RFC 2616 section 6.1.1
  (integer-in 100 599))

(define (http-response-standard-reason status-code)
  (hash-ref reason-lookup status-code #f))

(define (http-response->bytes response)
  (let ([output (open-output-bytes)])
    (define (write-line line)
      (write-bytes (string->bytes/utf-8 line) output)
      (write-bytes #"\r\n" output))
    ;; Status line (RFC 2616 section 6.1)
    (write-line
     (format
      "HTTP/~A.~A ~A ~A"
      (http-response-major-version response)
      (http-response-minor-version response)
      (http-response-status-code response)
      (http-response-reason response)))
    ;; Headers (RFC 2612 section 4.5, 6.2, 7.1)
    (for ([(header-name header-value) (in-hash (http-response-headers response))])
      (write-line
       (format
        "~A: ~A"
        header-name
        header-value)))
    ;; Blank line to terminate headers
    (write-line (string-empty))
    ;; Entity (RFC 2612 section 7.2)
    (let ([entity (http-response-entity response)])
      (when entity
        (write-bytes entity output)))
    ;; Return the result
    (get-output-bytes output)))