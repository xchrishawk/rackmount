;;
;; main.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the main entry point for the application. It is primarily
;; responsible for processing command line arguments and starting up all of the
;; worker threads in the main place.
;;

#lang racket

;; -- Requires --

(require "main/arguments.rkt")
(require "main/logging-thread.rkt")
(require "util/logging.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Main entry point for the application.
  [main (-> (listof string?) any)]))

;; -- Public Procedures --

(define (main args-list)
  (let* ([args (get-arguments args-list)])
    (parameterize ([minimum-log-event-level (arguments-minimum-log-event-level args)])
      (main-log-debug "Application launched. Arguments: ~A" (string-join args-list))
      (let ([logging-thread (logging-thread-start)])
        (main-log-trace "Application running. Waiting for break...")
        (wait-for-break)
        (main-log-trace "Break received, terminating application...")
        (logging-thread-stop logging-thread)))))

;; -- Private Procedures --

;; Waits for a break (CTRL-C).
(define (wait-for-break)
  (with-handlers ([exn:break? void])
    (sync/enable-break never-evt)))

;; Local logging procedures
(define-local-log main "Main")

;; -- Main Module --

(module+ main
  (main (vector->list (current-command-line-arguments))))
