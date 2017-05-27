;;
;; client.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This file defines the client task, which is primarily responsible for
;; communicating with a single client for a single HTTP transaction.
;;

#lang racket

;; -- Requires --

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

   ;; Cleans up the task without starting it.
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
  (displayln "CLIENT STARTED")
  (let loop ()
    (let ([evt (sync (thread-receive-evt) (read-line-evt input-port 'any))])
      (cond
        [(string? evt)
         (displayln (format "RX: ~A" evt))
         (loop)]
        [(equal? evt eof)
         (displayln (format "Client fucked off"))]
        [(and (equal? evt (thread-receive-evt))
              (equal? (thread-receive) 'shutdown))
         (displayln "Got shutdown event")]
        [else
         (error "got something weird" evt)])))
  ;; Clean up
  (close-input-port input-port)
  (close-output-port output-port)
  (displayln "CLIENT DONE"))
