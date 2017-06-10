;;
;; http-request.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures for parsing HTTP requests.

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; Struct representing an HTTP request.
  [struct http-request ([method (or/c string? false?)]
                        [uri (or/c string? false?)]
                        [major-version (or/c exact-nonnegative-integer? false?)]
                        [minor-version (or/c exact-nonnegative-integer? false?)]
                        [headers (or/c hash? false?)]
                        [body (or/c bytes? false?)])]

  ;; Creates a new, empty HTTP request object.
  [make-http-request (-> http-request?)]

  ;; Updates the specified HTTP request with the specified request line.
  [http-request-parse-request-line (-> http-request? string? (or/c http-request? false?))]

  ;; Updates the specified HTTP request with the specified header line.
  [http-request-parse-header-line (-> http-request? string? (or/c http-request? false?))]

  ;; Returns #t if the specified line indicates that there are no more headers.
  [http-request-is-end-header-line? (-> string? boolean?)]))

;; -- Structs --

(struct http-request (method
                      uri
                      major-version
                      minor-version
                      headers
                      body)
  #:transparent)

;; -- Public Procedures --

(define (make-http-request)
  (http-request #f #f #f #f #f #f))

(define (http-request-parse-request-line request line)
  (let* ([line-port (open-input-string line)]
         [method (read-from line-port token)]
         [uri (read-from line-port uri)]
         [version (read-from line-port http-version #:proc string->number)]
         [major-version (if version (first version) #f)]
         [minor-version (if version (second version) #f)])
    (if (and method uri major-version minor-version)
        (struct-copy
         http-request
         request
         [method method]
         [uri uri]
         [major-version major-version]
         [minor-version minor-version])
        #f)))

(define (http-request-parse-header-line request line)
  (let* ([original-headers
          (let ([headers (http-request-headers request)])
            (if (hash? headers) headers (hash)))]
         [line-port (open-input-string line)]
         [name (read-from line-port token)]
         [value (read-from line-port header-value)])
    (if (and name value)
        (struct-copy
         http-request
         request
         [headers (hash-set original-headers name value)])
        #f)))

(define (http-request-is-end-header-line? line)
  (zero? (string-length line)))

;; -- Private Utility (Read Primitives) --

(define (read-from port
                   token
                   #:skip-space [skip-space #t]
                   #:proc [proc #f])
  ;; Skip leading whitespace, if needed
  (when skip-space
    (space port))
  (let ([result (token port)])
    (if result
        (let* ([string-result (map bytes->string/utf-8 result)]
               [processed-result (if proc (map proc string-result) string-result)])
          (if (null? (rest processed-result))
              (first processed-result)
              processed-result))
        #f)))

(define (reader re)
  (λ (port)
    (let ([result (regexp-try-match re port)])
      (if result (rest result) #f))))

(define space
  (reader #px#"^([[:space:]]*)"))

(define token
  ;; RFC 2612, section 2.2
  (reader #px#"^([\x21\x23-\x27\x2A\x2B\x2D\x2E\x30-\x39\x41-\x5A\x5E-\x7A\x7C\x7E]+)"))

(define uri
  ;; TODO - formal regex for URI
  (reader #px#"^([[:graph:]]+)"))

(define http-version
  ;; RFC 2612, section 3.1
  (reader #px#"^HTTP/([[:digit:]]+)\\.([[:digit:]]+)"))

(define header-value
  ;; RFC 2612, section 4.2
  (reader #px#"(?m:^:[[:space:]]*(.*?)[[:space:]]*$)"))
