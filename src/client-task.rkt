;;
;; client-task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This file defines the client task, which is primarily responsible for
;; communicating with a single client for a single HTTP transaction.
;;

#lang racket

;; -- Requires --

(require "client.rkt")
(require "log.rkt")
(require "task.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Creates a client task spec using the specified values.
  [client-task-spec (-> string?					; client identifier
                        input-port?				; input port
                        output-port?				; output port
                        path-string?				; working directory
                        (or/c (and/c number? positive?) false?)	; client timeout
                        client-task-spec?)]

  ;; Predicate returning #t if the specified value is a client task spec.
  [client-task-spec? (-> any/c boolean?)]

  ;; Creates a new client task object from the specified task spec.
  [client-task (-> client-task-spec? task?)]))

;; -- Structs --

(struct opaque-client-task (thread
                            identifier
                            input-port
                            output-port
                            working-dir
                            timeout)
  #:mutable
  #:methods gen:task
  [;; Starts the task.
   (define (gen-task-start task)
     (let ([thd (thread (λ () (client-proc (opaque-client-task-identifier task)
                                           (opaque-client-task-input-port task)
                                           (opaque-client-task-output-port task)
                                           (opaque-client-task-working-dir task)
                                           (opaque-client-task-timeout task))))])
       (set-opaque-client-task-thread! task thd)))

   ;; Task was rejected - close the ports.
   (define (gen-task-reject task)
     (close-input-port (opaque-client-task-input-port task))
     (close-output-port (opaque-client-task-output-port task)))

   ;; Cancels the task and optionally waits for thread to terminate
   (define (gen-task-cancel task #:synchronous [synchronous #t])
     (let ([thd (opaque-client-task-thread task)])
       (if thd
           (begin
             (thread-send thd 'shutdown)
             (when synchronous
               (sync (gen-task-completed-evt task))))
           (error "Task has not been started!"))))

   ;; Returns an event ready when the task has completed.
   (define (gen-task-completed-evt task)
     (let ([thd (opaque-client-task-thread task)])
       (if thd
           (wrap-evt thd (λ (thd) task))
           (error "Task has not been started!"))))])

;; -- Public Procedures --

(define (client-task-spec identifier input-port output-port working-dir client-timeout)
  (list 'client-task identifier input-port output-port working-dir client-timeout))

(define (client-task-spec? x)
  (match x
    [(list 'client-task
           string?
           input-port?
           output-port?
           path-string?
           (and number? positive?))
     #t]
    [else #f]))

(define (client-task spec)
  (apply opaque-client-task #f (rest spec)))
