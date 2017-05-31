;;
;; manager-thread.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Defines the manager thread, which is responsible for coordinating a set of
;; worker places which provide parallelism for the server.
;;

#lang racket

;; -- Requires --

(require "../main/define-thread.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Configuration struct for the manager thread.
  [struct manager-thread-config ([placeholder any/c])]))

;; -- Types --

(struct manager-thread-config (placeholder)
  #:transparent)

;; -- Public Procedures --

(define-thread
  manager-thread
  manager-thread-config
  manager-thread-proc)

;; -- Private Procedures --

;; Main procedure for the thread.
(define (manager-thread-proc config)
  (manager-log-trace "Manager thread started.")
  (let loop ()
    (match (sync (wrapped-thread-receive-evt))
      ['shutdown (void)]))
  (manager-log-trace "Manager thread terminating."))

;; Local logging procedure.
(define-local-log manager "Manager")
