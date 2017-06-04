;;
;; example-task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines an example task for testing purposes.
;;

#lang racket

;; -- Requires --

(require "../tasks/task.rkt")
(require "../util/logging.rkt")
(require "../util/thread-util.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Task handle struct for the example task.
  [struct example-task-handle ([identifier gen:task-identifier?]
                               [duration (or/c (and/c real? positive?) false?)])]))

;; -- Types --

(struct example-task-handle (identifier duration)
  #:transparent
  #:methods gen:task-handle
  [(define (gen:task-handle-identifier task-handle)
     (example-task-handle-identifier task-handle))

   (define (gen:task-handle-initialize task-handle)
     (example-task-log-trace
      (example-task-handle-identifier task-handle)
      "Initializing task!"))

   (define (gen:task-handle-close task-handle)
     (example-task-log-trace
      (example-task-handle-identifier task-handle)
      "Closing task!"))

   (define (gen:task-handle->gen:task task-handle)
     (example-task
      (example-task-handle-identifier task-handle)
      (example-task-handle-duration task-handle)))])

(struct example-task (identifier
                      duration
                      [thread #:auto #:mutable])
  #:auto-value #f
  #:methods gen:task
  [(define (gen:task-identifier task)
     (example-task-identifier task))

   (define (gen:task-start task)
     (let* ([identifier (example-task-identifier task)]
            [duration (example-task-duration task)]
            [thd (thread-start
                  (example-task-log-trace identifier "Task started.")
                  (if (sync/timeout duration (thread-receive-evt))
                      (example-task-log-trace identifier "Task cancelled.")
                      (example-task-log-trace identifier "Task completed.")))])
       (set-example-task-thread! task thd)))

   (define (gen:task-cancel task #:synchronous [synchronous #t])
     (let ([thd (example-task-thread task)])
       (when thd
         (thread-send thd 'terminate)
         (when synchronous
           (sync thd))))
     (void))

   (define (gen:task-completed-evt task)
     (let ([thd (example-task-thread task)])
       (if thd
           (wrap-evt thd (λ (evt) task))
           never-evt)))])

;; -- Private Procedures --

;; Local logging procedures.
(define-local-log example-task "Example Task" #:require-identifier #t)
