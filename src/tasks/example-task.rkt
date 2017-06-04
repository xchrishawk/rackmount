;;
;; example-task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines an example task for testing purposes.
;;

#lang racket

;; -- Requires --

(require "../tasks/task.rkt")

;; -- Types --

(struct example-task-handle (identifier message)
  #:transparent
  #:methods gen:task-handle
  [(define (gen:task-handle-identifier task-handle)
     (example-task-handle-identifier task-handle))
   (define (gen:task-handle-initialize task-handle)
     (displayln
      (format "Initializing task ~A!" (example-task-handle-identifier task-handle))))
   (define (gen:task-handle-close task-handle)
     (displayln
      (format "Closing task ~A!" (example-task-handle-identifier task-handle))))
   (define (gen:task-handle->gen:task task-handle)
     (example-task
      (example-task-handle-identifier task-handle)
      (example-task-handle-message task-handle)))])

(struct example-task (identifier message)
  #:methods gen:task
  [(define (gen:task-identifier task)
     (example-task-identifier task))
   (define (gen:task-start task)
     (void))
   (define (gen:task-cancel task #:synchronous [synchronous #t])
     (void))
   (define (gen:task-completed-evt task)
     always-evt)])
