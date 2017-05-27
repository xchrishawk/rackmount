;;
;; task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines a generic interface for structs representing a task which
;; can be managed by a worker place.
;;

#lang racket

;; -- Requires --

(require racket/generic)

;; -- Provides --

(provide

 ;; Generic interface for types representing a task that a worker place can run.
 gen:task

 (contract-out

  ;; Returns #t if the argument implements the gen:task interface.
  [task? (-> any/c boolean?)]

  ;; Starts the specified task.
  [gen-task-start (-> task? any)]

  ;; Rejects the specified task. The task should perform any required cleanup
  ;; without actually starting processing.
  [gen-task-reject (-> task? any)]

  ;; Cancels the specified task. Must not be called unless the task has actually
  ;; been started.
  ;;
  ;; If synchronous is #t, this method will block until the (gen-task-completed-evt)
  ;; event for this task is ready for synchronization.
  [gen-task-cancel (->* (task?) (#:synchronous boolean?) any)]

  ;; Returns an event which is ready for synchronization when the task has
  ;; completed. The synchronization result is the task object iself.
  [gen-task-completed-evt (-> task? evt?)]))

;; -- Interfaces --

(define-generics task
  (gen-task-start task)
  (gen-task-reject task)
  (gen-task-cancel task #:synchronous [synchronous])
  (gen-task-completed-evt task))
