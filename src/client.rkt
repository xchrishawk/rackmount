;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This file defines the client task, which is primarily responsible for
;; communicating with a single client for a single HTTP transaction.
;;

#lang racket

;; -- Requires --

(require "log.rkt")
(require "task.rkt")

;; -- Provides --

(provide
 (contract-out
  ;; Creates a new client task object.
  [make-client-task (-> input-port? output-port? task?)]))

;; -- Structs --

(struct client-task (thread input-port output-port)
  #:mutable
  #:methods gen:task
  [;; Starts the task.
   (define (gen-task-start task)
     (let ([thd (thread (λ ()
                          (client-proc (client-task-input-port task)
                                       (client-task-output-port task))))])
       (set-client-task-thread! task thd)))

   ;; Task was rejected - close the ports.
   (define (gen-task-reject task)
     (close-input-port (client-task-input-port task))
     (close-output-port (client-task-output-port task)))

   ;; Cancels the task and optionally waits for thread to terminate
   (define (gen-task-cancel task #:synchronous [synchronous #t])
     (let ([thd (client-task-thread task)])
       (if thd
           (begin
             (thread-send thd 'shutdown)
             (when synchronous
               (sync (gen-task-completed-evt task))))
           (error "Task has not been started!"))))

   ;; Returns an event ready when the task has completed.
   (define (gen-task-completed-evt task)
     (let ([thd (client-task-thread task)])
       (if thd
           (wrap-evt thd (λ (thd) task))
           (error "Task has not been started!"))))])

;; -- Public Procedures --

(define (make-client-task input-port output-port)
  (client-task #f input-port output-port))

;; -- Private Procedures --

(define (client-proc input-port output-port)
  (client-log "Client connected...")
  (let loop ()
    (let ([evt (sync (thread-receive-evt) (read-line-evt input-port 'any))])
      (match evt

        ;; Client sent a new line to process
        [(? string? line)
         (client-log "RX: ~A" line)
         (displayln (format "You sent: ~A" line) output-port)
         (flush-output output-port)
         (loop)]

        ;; Client disconnected on their end
        [(? (λ (evt) (equal? evt eof)) _)
         (client-log "Client disconnected on remote end.")]

        ;; Thread received a message
        [(? (λ (evt) (equal? evt (thread-receive-evt))) evt)
         (match (thread-receive)
           ;; Shutdown command
           ['shutdown
            (client-log "Received shutdown command, terminating client...")]
           ;; Unknown command?
           [else
            (client-log "Received unknown command, ignoring...")
            (loop)])]

        ;; Unknown event?
        [else
         (client-log "Received unknown event (~A), ignoring..." evt)
         (loop)])))

  ;; Clean up
  (close-input-port input-port)
  (close-output-port output-port)
  (client-log "Client connection terminated."))

;; Logs an event to the "Client" category.
(define client-log
  (create-local-log "Client"))
