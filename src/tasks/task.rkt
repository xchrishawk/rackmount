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

  ;; Serializes this task handle to a place message.
  [gen:task-handle->place-message (-> task-handle? (listof place-message-allowed?))]

  ;; Deserializes a place message to a task handle.
  [place-message->gen:task-handle (-> (listof place-message-allowed?) task-handle?)]

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
  ;; is no longer running.
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

(define (gen:task-handle->place-message task-handle)
  (let*-values ([(name
                  init-field-cnt
                  auto-field-cnt
                  accessor-proc
                  mutator-proc
                  immutable-k-list
                  super-type
                  skipped?)
                 (let-values ([(info skipped?) (struct-info task-handle)])
                   (if info
                       (struct-type-info info)
                       (error "Type cannot be serialized!")))])
    (cons name
          (build-list
           (+ init-field-cnt auto-field-cnt)
           (λ (index)
             (let ([value (accessor-proc task-handle index)])
               (if (place-message-allowed? value)
                   value
                   (error "Type contains non-serializable value!"))))))))

(define (place-message->gen:task-handle place-message)
  (let ([struct-constructor (eval (first place-message))])
    (apply struct-constructor (rest place-message))))

;; -- Public Procedures (Task Interface) --

(define-generics task
  (gen:task-identifier task)
  (gen:task-start task)
  (gen:task-cancel task #:synchronous [synchronous])
  (gen:task-completed-evt task))

;; -- Public Procedures (Common) --

(define gen:task-identifier? string?)
