;;
;; uri.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures for working with URIs as defined by
;; RFC 2396.
;;

#lang racket

;; -- Requires --

(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Struct representing a URI.
  [struct uri ([string string?]
               [scheme (maybe/c string?)]
               [authority (maybe/c string?)]
               [path (maybe/c string?)]
               [query (maybe/c string?)]
               [fragment (maybe/c string?)])]

  ;; Parses a URI struct from a string.
  [make-uri (-> string? uri?)]

  ;; Extracts a port number from the authority field of the specified URI.
  [uri-port-number (-> uri? (maybe/c port-number?))]))

;; -- Structs --

(struct uri (string
             scheme
             authority
             path
             query
             fragment)
  #:transparent)

;; -- Public Procedures --

(define (make-uri uri-string)
  (let-values ([(string g1 scheme g3 authority path g6 query g8 fragment)
                (apply values (regexp-match
                               ;; RFC 2396 appendix B
                               #px"^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
                               uri-string))])
    (uri string scheme authority path query fragment)))

(define (uri-port-number uri)
  (ifmap ([authority (uri-authority uri)])
    (ifmap ([result (regexp-match #px"^.+:([0-9]+)$" authority)])
      (let ([port-number (string->number (second result))])
        (if-is port-number? port-number)))))
