;;
;; client-task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the client task handle and task objects, which are used to
;; coordinate execution of client handling procedures on worker threads.
;;

#lang racket

;; -- Requires --

(require "../tasks/task.rkt")
(require "../util/logging.rkt")
(require "../util/thread-util.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Task handle struct for client tasks.
  [struct client-task-handle ([identifier gen:task-identifier?]
                              [input-port input-port?]
                              [output-port output-port?]
                              [working-dir path-string?])]))

;; -- Types --

(struct client-task-handle (identifier
                            input-port
                            output-port
                            working-dir)
  #:transparent
  #:methods gen:task-handle
  [(define (gen:task-handle-identifier task-handle)
     (client-task-handle-identifier task-handle))

   (define (gen:task-handle-initialize task-handle)
     (client-task-log-trace
      (client-task-handle-identifier task-handle)
      "Client task initialized."))

   (define (gen:task-handle-close task-handle)
     (close-input-port (client-task-handle-input-port task-handle))
     (close-output-port (client-task-handle-output-port task-handle))
     (client-task-log-trace
      (client-task-handle-identifier task-handle)
      "Client task closed. Socket connection terminated."))

   (define (gen:task-handle->gen:task task-handle)
     (client-task (client-task-handle-identifier task-handle)
                  (client-task-handle-input-port task-handle)
                  (client-task-handle-output-port task-handle)
                  (client-task-handle-working-dir task-handle)))])

(struct client-task (identifier
                     input-port
                     output-port
                     working-dir
                     [thread #:auto #:mutable])
  #:auto-value #f
  #:methods gen:task
  [(define (gen:task-identifier task)
     (client-task-identifier task))

   (define (gen:task-start task)
     (let ([thd (thread-start
                 (sync/timeout 5.0 (thread-receive-evt)))])
       (set-client-task-thread! task thd)))

   (define (gen:task-cancel task #:synchronous [synchronous #t])
     (let ([thd (client-task-thread task)])
       (when (and thd (thread-running? thd))
         (thread-send thd 'terminate)
         (when synchronous
           (sync thd))))
     (void))

   (define (gen:task-completed-evt task)
     (let ([thd (client-task-thread task)])
       (if thd
           (wrap-evt thd (thunk* task))
           never-evt)))])

;; -- Private Procedures --

(define-local-log client-task "Client Task" #:require-identifier #t)
