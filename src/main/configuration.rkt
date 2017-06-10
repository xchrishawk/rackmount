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

  ;; -- Parameters --

  ;; Parameter containing the minimum log event level which will be reported.
  [config-minimum-log-event-level (parameter/c symbol?)]

  ;; Parameter containing the server name which will be reported to clients.
  [config-server-name (parameter/c string?)]

  ;; The timeout to use for client sessions.
  [config-session-timeout (parameter/c session-timeout?)]

  ;; The current working directory for the server.
  [config-working-dir (parameter/c path-string? (or/c path-string? false?))]

  ;; -- Procedures --

  ;; Predicate returning #t if the argument is a valid session timeout.
  [session-timeout? (-> any/c boolean?)]))

;; -- Parameters --

(define config-minimum-log-event-level
  (make-parameter 'trace))

(define config-server-name
  (make-parameter (rackmount-version)))

(define config-session-timeout
  (make-parameter #f))

(define config-working-dir
  (make-parameter #f))

;; -- Public Procedures --

(define session-timeout?
  (or/c (and/c real? positive?) false/c))
