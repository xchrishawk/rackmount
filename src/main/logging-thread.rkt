;;
;; logging-thread.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the main place's logging thread, which is responsible for
;; dequeuing and processing events logged with the log queue.
;;

#lang racket

;; -- Requires --

(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Starts and returns a logging thread.
  [logging-thread-start (-> opaque-logging-thread?)]

  ;; Synchronously stops the specified logging thread.
  [logging-thread-stop (-> opaque-logging-thread? void?)]))

;; -- Types --

(struct opaque-logging-thread (thread))

;; -- Public Procedures --

(define (logging-thread-start)
  (opaque-logging-thread (thread logging-thread-proc)))

(define (logging-thread-stop logging-thread)
  (let ([thd (opaque-logging-thread-thread logging-thread)])
    (thread-send thd 'shutdown)
    (sync thd))
  (void))

;; -- Private Procedures --

(define (logging-thread-proc)
  (let loop ()
    (match (sync (wrapped-thread-receive-evt)
                 (log-event-dequeue-evt))
      ;; Received shutdown event - flush all remaining events then stop looping
      ['shutdown
       (let flush-all-loop ()
         (let ([log-event (log-event-dequeue)])
           (when log-event
             (report-log-event log-event)
             (flush-all-loop))))]
      ;; Received event - report it
      [(? log-event? log-event)
       (report-log-event log-event)
       (loop)])))

;; Reports a log event. For now, just print it to the output port. Eventually they
;; will get saved to some type of persistent store.
(define (report-log-event log-event)
  (displayln (log-event->string log-event))
  (newline))
