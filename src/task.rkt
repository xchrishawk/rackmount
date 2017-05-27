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
 gen:task
 (contract-out
  ;; Returns #t if the argument implements the gen:task interface.
  [task? (-> any/c boolean?)]
  ;; Starts the specified task.
  [gen-task-start (-> task? any)]
  ;; Cancels the specified task.
  [gen-task-cancel (-> task? any)]
  ;; Returns an event which is ready for synchronization when the task has completed.
  [gen-task-completed-evt (-> task? evt?)]))

;; -- Interfaces --

(define-generics task
  (gen-task-start task)
  (gen-task-cancel task)
  (gen-task-completed-evt task))
