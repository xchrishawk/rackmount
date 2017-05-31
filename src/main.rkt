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
(require "main/listener-thread.rkt")
(require "main/logging-thread.rkt")
(require "main/manager-thread.rkt")
(require "util/logging.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Main entry point for the application.
  [main (-> (listof string?) any)]))

;; -- Public Procedures --

(define (main args-list)
  (let* ([args (get-arguments args-list)])
    (main-log-info "Starting with arguments: ~A" (string-join args-list))
    (let* ([logging-thread-config
            (logging-thread-config (arguments-minimum-log-event-level args))]
           [logging-thread
            (logging-thread-start logging-thread-config)]
           [manager-thread-config
            (manager-thread-config #f)]
           [manager-thread
            (manager-thread-start manager-thread-config)]
           [listener-thread-config
            (listener-thread-config (arguments-working-dir args)
                                    (arguments-interface args)
                                    (arguments-port-number args)
                                    4    ; max wait count
                                    #t)] ; reusable
           [listener-thread
            (listener-thread-start listener-thread-config)])
      (main-log-debug "Startup complete. Waiting for break...")
      (wait-for-break)
      (main-log-info "Break received, terminating application...")
      (listener-thread-stop listener-thread)
      (manager-thread-stop manager-thread)
      (logging-thread-stop logging-thread))))

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
