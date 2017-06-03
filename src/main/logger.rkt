;;
;; logger.rkt
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
  [struct logger-config ([minimum-log-event-level log-event-level?])]))

;; -- Types --

(struct logger-config (minimum-log-event-level)
  #:transparent)

;; -- Public Procedures --

(define-thread
  logger
  logger-config
  logger-proc)

;; -- Private Procedures --

;; Main thread for the logger.
(define (logger-proc config)
  (let loop ()
    (sync
     ;; Received thread message.
     (handle-evt
      (wrapped-thread-receive-evt)
      (match-lambda
        ['shutdown (void)]
        [unrecognized-message (loop)]))
     ;; Received logged event.
     (handle-evt
      (log-event-dequeue-evt)
      (λ (log-event)
        (proc-log-event config log-event)
        (loop))))))

;; Processes a logged event. For now, just print it to the output port. Eventually they
;; will get saved to some type of persistent store.
(define (proc-log-event config log-event)
  (when (log-event-level-enabled? (log-event-level log-event)
                                  (logger-config-minimum-log-event-level config))
    (displayln (log-event->string log-event))
    (newline)))
