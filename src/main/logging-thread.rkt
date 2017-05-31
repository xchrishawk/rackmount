;;
;; logging-thread.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the main place's logging thread, which is responsible for
;; dequeuing and processing events logged with the log queue.
;;

#lang racket

;; -- Requires --

(require "../main/define-thread.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Configuration struct for the logging thread.
  [struct logging-thread-config ([minimum-log-event-level log-event-level?])]))

;; -- Types --

(struct logging-thread-config (minimum-log-event-level)
  #:transparent)

;; -- Public Procedures --

(define-thread
  logging-thread
  logging-thread-config
  logging-thread-proc)

;; -- Private Procedures --

(define (logging-thread-proc config)
  (let loop ()
    (match (sync (wrapped-thread-receive-evt)
                 (log-event-dequeue-evt))
      ;; Received shutdown event - flush all remaining events then stop looping
      ['shutdown
       (let flush-all-loop ()
         (let ([log-event (log-event-dequeue)])
           (when log-event
             (report-log-event log-event config)
             (flush-all-loop))))]
      ;; Received event - report it
      [(? log-event? log-event)
       (report-log-event log-event config)
       (loop)])))

;; Reports a log event. For now, just print it to the output port. Eventually they
;; will get saved to some type of persistent store.
(define (report-log-event log-event config)
  (when (log-event-level-enabled? (log-event-level log-event)
                                  (logging-thread-config-minimum-log-event-level config))
    (displayln (log-event->string log-event))
    (newline)))
