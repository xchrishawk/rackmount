;;
;; task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Class defining a generic interface for tasks which may be queued with one of
;; the manager's worker places.
;;
;; The "task handle" represents a request for a task. It is tracked on the manager
;; side of the application. It may be converted to a list representation for
;; transmission to a worker place.
;;
;; The "task" object represents the running task itself. It is intended for use
;; on the worker side of the application.
;;

#lang racket

;; -- Requires --

(require racket/generic)

;; -- Provides --

(provide

 ;; -- Task Handles --

 ;; Generic interface for objects representing a task handle.
 gen:task-handle

 (contract-out

  ;; Predicate returning #t if the argument implements the gen:task-handle interface.
  [rename task-handle? gen:task-handle? (-> any/c boolean?)]

  ;; Returns an identifier for this task handle.
  [gen:task-handle-identifier (-> task-handle? any/c)]

  ;; Performs manager-side cleanup of the task after it has completed.
  [gen:task-handle-close (-> task-handle? any)]

  ;; Converts the task handle to a list which may be transmitted over a place channel.
  [gen:task-handle->list (-> task-handle? (listof place-message-allowed?))])

 ;; -- Tasks --

 ;; Generic interface for objects representing a task.
 gen:task

 (contract-out

  ;; Predicate returning #t if the argument implements the gen:task interface.
  [rename task? gen:task? (-> any/c boolean?)]

  ;; Returns an identifier for this task.
  [gen:task-identifier (-> task? any/c)]

  ;; Start a task.
  [gen:task-start (-> task? any)]

  ;; Cancels an in-progress task.
  [gen:task-cancel (-> task? any)]

  ;; Returns a synchronizable event which is ready for synchronization when the
  ;; task has completed running. The synchronization result is unspecified.
  [gen:task-completed-evt (-> task? evt?)]))

;; -- Public Procedures --

(define-generics task-handle
  (gen:task-handle-identifier task-handle)
  (gen:task-handle-close task-handle)
  (gen:task-handle->list task-handle))

(define-generics task
  (gen:task-identifier task)
  (gen:task-start task)
  (gen:task-cancel task)
  (gen:task-completed-evt task))
