;;
;; task.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require racket/generic)

;; -- Provides --

(provide

 ;; -- Interfaces --

 gen:task-handle

 gen:task

 (contract-out

  ;; -- Variables --

  ;; The inspector to use for task handle structs. Structs implementing the
  ;; gen:task-handle interface should define this as their inspector, otherwise
  ;; they will not be able to be serialized to be sent across place channels.
  [task-handle-inspector inspector?]

  ;; -- Task Handle Interface --

  ;; Predicate returning #t if the argument is a task handle object.
  [rename task-handle? gen:task-handle? (-> any/c boolean?)]

  ;; Returns the identifier for the specified task handle.
  [gen:task-handle-identifier (-> task-handle? gen:task-identifier?)]

  ;; Performs manager-side initialization of a task before it is assigned to a
  ;; worker place. This should only be called by the manager.
  [gen:task-handle-initialize (-> task-handle? any)]

  ;; Performs manager-side cleanup of a task after it has been completed by a
  ;; worker place. This should only be called by the manager.
  [gen:task-handle-close (-> task-handle? any)]

  ;; Converts this task handle to a corresponding task object.
  [gen:task-handle->gen:task (-> task-handle? task?)]

  ;; -- Task Interface --

  ;; Predicate returning #t if the argument is a task object.
  [rename task? gen:task? (-> any/c boolean?)]

  ;; Returns the identifier for the specified task.
  [gen:task-identifier (-> task? gen:task-identifier?)]

  ;; Launches this task in its own thread.
  [gen:task-start (-> task? any)]

  ;; Cancels this task. If #:synchronous is set to #t, this method will block
  ;; until the task's thread has terminated.
  [gen:task-cancel (->* (task?) (#:synchronous boolean?) any)]

  ;; Returns an event which is ready for synchronization when the task's thread
  ;; is no longer running. The synchronization result is the task object itself.
  [gen:task-completed-evt (-> task? evt?)]

  ;; -- Common --

  ;; Predicate returning #t if the argument is a valid task identifier.
  [gen:task-identifier? (-> any/c boolean?)]))

;; -- Variables --

(define task-handle-inspector (make-inspector))

;; -- Public Procedures (Task Handle Interface) --

(define-generics task-handle
  (gen:task-handle-identifier task-handle)
  (gen:task-handle-initialize task-handle)
  (gen:task-handle-close task-handle)
  (gen:task-handle->gen:task task-handle))

;; -- Public Procedures (Task Interface) --

(define-generics task
  (gen:task-identifier task)
  (gen:task-start task)
  (gen:task-cancel task #:synchronous [synchronous])
  (gen:task-completed-evt task))

;; -- Public Procedures (Common) --

(define gen:task-identifier? string?)
