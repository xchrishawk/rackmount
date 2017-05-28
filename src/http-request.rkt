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

  ;; Parses a request line.
  ;;
  ;; Returns either a list of (method uri major-version minor-version), or #f if
  ;; the request line could not be successfully parsed.
  [parse-request-line (-> string? (or/c list? false?))]))

;; -- Public Procedures --

(define (parse-request-line line-string)
  (let* ([line-port (open-input-string line-string)]
         [method (read-from line-port token)]
         [uri (read-from line-port uri)]
         [version (read-from line-port http-version #:proc string->number)]
         [major-version (if version (first version) #f)]
         [minor-version (if version (second version) #f)])
    (if (and method uri major-version minor-version)
        (list method uri major-version minor-version)
        #f)))

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
  ;; RFC 2512, section 3.1
  (reader #px#"^HTTP/([[:digit:]]+)\\.([[:digit:]]+)"))
