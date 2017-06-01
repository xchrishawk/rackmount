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
(require "main/listener.rkt")
(require "main/logger.rkt")
(require "main/manager.rkt")
(require "worker/client-task.rkt")
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
    (let* (;; Create logging thread
           [logger-config (make-logger-config args)]
           [logger (logger-start logger-config)]
           ;; Create manager thread
           [manager-config (make-manager-config args)]
           [manager (manager-start manager-config)]
           ;; Create listener thread
           [listener-config (make-listener-config args manager)]
           [listener (listener-start listener-config)])
      ;; Wait for break
      (main-log-debug "Startup complete. Waiting for break...")
      (wait-for-break)
      (main-log-info "Break received, terminating application...")
      ;; Shut down all of our threads
      (listener-stop listener)
      (manager-stop manager)
      (logger-stop logger))))

;; -- Private Procedures --

;; Waits for a break (CTRL-C).
(define (wait-for-break)
  (with-handlers ([exn:break? void])
    (sync/enable-break never-evt)))

;; Creates the configuration struct for the logger.
(define (make-logger-config args)
  (logger-config (arguments-minimum-log-event-level args)))

;; Creates the configuration struct for the worker manager.
(define (make-manager-config args)
  (manager-config (arguments-worker-count args)))

;; Creates the configuration struct for the listener.
(define (make-listener-config args manager)
  (let* ([identifier-generator (client-task-identifier-generator)]
         [client-connected (λ (input-port output-port)
                             (handle-client-connected manager
                                                      identifier-generator
                                                      input-port
                                                      output-port))])
    (listener-config (arguments-interface args)
                     (arguments-port-number args)
                     4   ; max wait count
                     #t  ; reusable
                     client-connected)))

;; Creates a client task handle and queues it with the manager.
(define (handle-client-connected manager
                                 identifier-generator
                                 input-port
                                 output-port)
  (let ([task-handle (client-task-handle (identifier-generator)
                                         input-port
                                         output-port)])
    (manager-queue-task-handle manager task-handle)))

;; Local logging procedures
(define-local-log main "Main")

;; -- Main Module --

(module+ main
  (main (vector->list (current-command-line-arguments))))
