;;
;; http-request.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require "exception.rkt")
(require "utility.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Parses an HTTP request string.
  [parse-http-request (-> string? http-request?)]))

(provide
 (contract-out
  ;; Struct representing an HTTP request.
  [struct http-request ([method string?]
                        [uri string?]
                        [version-major exact-nonnegative-integer?]
                        [version-minor exact-nonnegative-integer?]
                        [headers (hash/c string? string?)])]))

;; -- Structs --

(struct http-request (method
                      uri
                      version-major
                      version-minor
                      headers)
  #:transparent)

;; -- Public Procedures --

(define (parse-http-request request-string)
  (let*-values ([(request) (open-input-string request-string)]
                [(method) (mandatory read-token request "Method")]
                [(uri) (mandatory read-uri request "URI")]
                [(full-version major-version-string minor-version-string)
                 (mandatory read-version request "Version")]
                [(major-version) (string->version major-version-string)]
                [(minor-version) (string->version minor-version-string)]
                [(headers) (read-headers request)])
    (http-request method uri major-version minor-version headers)))

;; -- Private Procedures (Parsing) --

(define (read-headers request)
  (let loop ([headers (hash)])
    (match (read-header request)
      [(list key value) (loop (hash-set headers key value))]
      [#f headers])))

(define (string->version str)
  (let ([number (string->number str)])
    (when (not (exact-positive-integer? number))
      (error "Invalid HTTP version string:" str))
    number))

;; -- Private Procedures (Reading) --

(define (mandatory proc request token-type)
  (let ([result (proc request)])
    (if result
        (apply values result)
        (raise-rackmount-bad-http-request-error
         "Failed to read token - expected ~A, got \"~A\""
         token-type
         (read-line request 'any)))))

(define (reader regex . indices)
  (λ (request)
    (let ([indices (if (not (empty? indices)) indices (list 1))]
          [result (regexp-try-match regex request)])
      (if result
          (map bytes->string/utf-8 (apply list-refs result indices))
          #f))))

(define read-token
  ;; RFC2612 section 2.2
  (reader #px#"^[[:space:]]*([\x21\x23-\x27\x2A\x2B\x2D\x2E\x30-\x39\x41-\x5A\x5E-\x7A\x7C\x7E]+)"))

(define read-uri
  ;; TODO...
  (reader #px#"^[[:space:]]*([[:graph:]]+)"))

(define read-version
  ;; RFC2612 section 3.1
  (reader #px#"^[[:space:]]*(HTTP/([0-9]+)\\.([0-9]+))" 1 2 3))

(define read-header
  (reader #px#"(?m:^([\x21\x23-\x27\x2A\x2B\x2D\x2E\x30-\x39\x41-\x5A\x5E-\x7A\x7C\x7E]+):\
[[:space:]]*(.*?)[[:space:]]*$)" 1 2))
