;;
;; http-request.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures for parsing HTTP requests.

#lang racket

;; -- Requires --

(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct representing an HTTP request.
  ;; - note: Fields are optional because this struct is generated iteratively as
  ;;   we receive lines from the client.
  [struct http-request ([raw string?]
                        [method (maybe/c string?)]
                        [uri (maybe/c string?)]
                        [major-version (maybe/c exact-nonnegative-integer?)]
                        [minor-version (maybe/c exact-nonnegative-integer?)]
                        [headers hash?]
                        [body (maybe/c bytes?)])]

  ;; Creates a new, empty HTTP request object.
  [make-http-request (-> http-request?)]

  ;; Updates the specified HTTP request with the specified request line. Returns
  ;; a validity flag and the updated struct.
  [http-request-parse-request-line
   (-> http-request?
       string?
       (values boolean? http-request?))]

  ;; Updates the specified HTTP request with the specified header line. Returns
  ;; a validity flag and the updated struct.
  [http-request-parse-header-line
   (-> http-request?
       string?
       (values boolean? http-request?))]

  ;; Returns #t if the specified line indicates that there are no more headers.
  [http-request-is-end-header-line? (-> string? boolean?)]))

;; -- Structs --

(struct http-request (raw
                      method
                      uri
                      major-version
                      minor-version
                      headers
                      body)
  #:transparent)

;; -- Public Procedures --

(define (make-http-request)
  (http-request (string)
                #f
                #f
                #f
                #f
                (hash)
                #f))

(define (http-request-parse-request-line request line)
  (let* ([line-port (open-input-string line)]
         [raw (update-raw request line)]
         [method (read-from line-port token)]
         [uri (read-from line-port uri)]
         [version (read-from line-port http-version #:proc string->number)]
         [major-version (if version (first version) #f)]
         [minor-version (if version (second version) #f)])
    (if (and method uri major-version minor-version)
        (values #t (struct-copy
                    http-request
                    request
                    [raw raw]
                    [method method]
                    [uri uri]
                    [major-version major-version]
                    [minor-version minor-version]))
        (values #f (struct-copy
                    http-request
                    request
                    [raw raw])))))

(define (http-request-parse-header-line request line)
  (let* ([line-port (open-input-string line)]
         [raw (update-raw request line)]
         [header-name (read-from line-port token)]
         [header-value (read-from line-port header-value)])
    (if (and header-name header-value)
        (values #t (struct-copy
                    http-request
                    request
                    [raw raw]
                    [headers (hash-set
                              (http-request-headers request)
                              header-name
                              header-value)]))
        (values #f (struct-copy
                    http-request
                    request
                    [raw raw])))))

(define (http-request-is-end-header-line? line)
  (string-empty? line))

;; -- Private Utility (Misc) --

;; Appends a line to the raw string for the specified request.
(define (update-raw request line)
  (let ([original-raw (http-request-raw request)])
    (if (string-empty? original-raw)
        line
        (string-append original-raw "\n" line))))

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
