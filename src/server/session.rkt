;;
;; session.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the state machine used to handle interactions with an HTTP
;; client.
;;

#lang racket

;; -- Requires --

(require "../tasks/task.rkt")
(require "../util/logging.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Main client handler procedure.
  [session-proc (-> gen:task-identifier? input-port? output-port? path-string? void?)]))

;; -- Public Procedures --

(define (session-proc identifier input-port output-port working-dir)
  (void))

;; -- Private Procedures --

(define-local-log session "Session" #:require-identifier #t)
