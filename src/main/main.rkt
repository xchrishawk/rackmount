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

(require racket/generator)
(require "../main/arguments.rkt")
(require "../main/listener.rkt")
(require "../tasks/client-task.rkt")
(require "../tasks/manager.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Main entry point for the application.
  [main (-> (listof string?) any)]))

;; -- Objects --

(define client-identifier-generator
  (sequence->generator (in-naturals)))

;; -- Public Procedures --

(define (main args-list)
  (let* ([args (parse-arguments args-list)])
    (main-log-info "Starting with arguments: ~A" (string-join args-list))
    (let* (;; Start manager
           [manager-config (make-manager-config args)]
           [manager (manager manager-config)]
           ;; Start listener
           [listener-config (make-listener-config args manager)]
           [listener (listener listener-config)])
      (main-log-debug "Startup complete. Waiting for break...")
      (wait-for-break)
      (main-log-info "Break received, terminating application...")
      (listener-terminate listener)
      (manager-terminate manager))))

;; -- Private Procedures --

;; Waits for a break (CTRL-C).
(define (wait-for-break)
  (with-handlers ([exn:break? void])
    (sync/enable-break never-evt)))

;; Creates the configuration struct for the worker manager.
(define (make-manager-config args)
  (manager-config (arguments-worker-count args)))

;; Creates the configuration struct for the listener.
(define (make-listener-config args manager)
  (listener-config
   (arguments-interface args)
   (arguments-port-number args)
   4   ; max wait count
   #t  ; reusable
   (λ (input-port output-port)
     (handle-client-connected
      manager
      input-port
      output-port
      (arguments-working-dir args)))))

;; Creates a client task handle and queues it with the manager.
(define (handle-client-connected manager input-port output-port working-dir)
  (let ([task-handle (client-task-handle
                      (next-client-identifier)
                      input-port
                      output-port
                      working-dir)])
    (manager-enqueue manager task-handle)))

(define (next-client-identifier)
  (format "Client ~A" (client-identifier-generator)))

;; Local logging procedures
(define-local-log main "Main")

;; -- Main Module --

(module+ main
  (let ([args-list (vector->list (current-command-line-arguments))])
    (main args-list)))
