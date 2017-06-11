;;
;; http-request-handle.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module is responsible for dispatching HTTP requests to the correct handler
;; procedure.
;;

#lang racket

;; -- Requires --

(require "../http/http-request.rkt")
(require "../http/http-request-handle-get.rkt")
(require "../http/http-response.rkt")
(require "../http/http-response-lib.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Processes an HTTP request and returns a response.
  [http-request-handle (-> http-request? http-response?)]))

;; -- Public Procedures --

(define (http-request-handle request)
  (match (http-request-method request)
    ;; RFC 2612 section 9.3 - GET request
    ["GET" (http-request-handle-get request)]
    ;; RFC 2612 section 10.5.2 - we don't recognize the request. Send a 501 error.
    [else (http-response-not-implemented)]))
