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
  [(define (gen-task-start task)
     (let ([thd (thread (λ () (client-proc (client-task-input-port task)
                                           (client-task-output-port task))))])
       (set-client-task-thread! task thd)))
   (define (gen-task-cancel task)
     (let ([thd (client-task-thread task)])
       (if thd
           (thread-send thd 'shutdown)
           (error "Task has not been started!"))))
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
  (let loop ()
    (let ([evt (sync (thread-receive-evt) (read-line-evt input-port 'any))])
      (match evt
        [(? string? line)
         (displayln (format "RX: ~A" line))
         (displayln (format "You said: ~A" line) output-port)
         (loop)]
        [eof (void)]
        ['shutdown (void)]))))
