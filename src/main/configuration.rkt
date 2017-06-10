;;
;; configuration.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines parameters used to configure the server.
;;

#lang racket

;; -- Requires --

(require "../main/version.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Parameter containing the minimum log event level which will be reported.
  [minimum-log-event-level parameter/c]

  ;; Parameter containing the server name which will be reported to clients.
  [server-name (parameter/c string?)]))

;; -- Parameters --

(define minimum-log-event-level
  (make-parameter 'trace))

(define server-name
  (make-parameter (rackmount-version)))
