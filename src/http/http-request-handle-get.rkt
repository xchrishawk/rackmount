;;
;; http-request-handle-get.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module is responsible for handling HTTP GET requests.
;;

#lang racket

;; -- Requires --

(require "../http/http-request.rkt")
(require "../http/http-response.rkt")
(require "../http/http-response-lib.rkt")
(require "../http/uri.rkt")
(require "../main/configuration.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Handles a GET request.
  [http-request-handle-get (-> http-request? http-response?)]))

;; -- Public Procedures --

(define (http-request-handle-get request)
  (let* ([uri (make-uri (http-request-uri request))]
         [local-path (local-path-for-uri uri)])
    (cond
      ;; Path is a valid file
      [(file-exists? local-path)
       ;; TODO - obviously needs bettered
       (let ([file-port (open-input-file local-path)])
         (begin0
             (http-response-ok #:entity (port->bytes file-port))
           (close-input-port file-port)))]
      ;; Path is a valid directory - TODO
      [(directory-exists? local-path)
       (http-response-not-found)]
      ;; Path doesn't exist - return 404
      [else
       (http-response-not-found)])))

;; -- Private Procedures --

(define (local-path-for-uri uri)
  (let* ([uri-path (uri-path uri)]
         ;; build-path doesn't like the leading slash
         [trimmed-uri-path (string-trim uri-path "/" #:repeat? #t)])
    (apply
     build-path
     (filter
      (λ (str) (not (string-empty? str)))
      (list (config-working-dir) trimmed-uri-path)))))
