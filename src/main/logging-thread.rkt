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
    (match (sync (wrap-evt
                  (thread-receive-evt)
                  (λ (evt) (thread-receive)))
                 (log-event-dequeue-evt))
      ;; Received shutdown event - stop looping
      ['shutdown (void)]
      ;; Received event - for now, just print it to the output port. Eventually
      ;; this will get logged to a persistent store of some type.
      [(? log-event? log-event)
       (displayln (log-event->string log-event))
       (newline)
       (loop)])))
