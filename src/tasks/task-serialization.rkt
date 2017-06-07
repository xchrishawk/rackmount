;;
;; task-serialization.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines procedures for serializing and deserializing task handles
;; to messages which can be transmitted across a place channel.
;;

#lang racket

;; -- Requires --

(require "../tasks/client-task.rkt")
(require "../tasks/example-task.rkt")
(require "../tasks/task.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Serializes a task handle to a place message.
  [gen:task-handle->place-message (-> gen:task-handle? (listof place-message-allowed?))]

  ;; Deserializes a place message to a task handle.
  [place-message->gen:task-handle (-> (listof place-message-allowed?) gen:task-handle?)]))

;; -- Variables --

(define constructor-lookup
  (hash
   'client-task-handle client-task-handle
   'example-task-handle example-task-handle))

;; -- Public Procedures --

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
  (let ([struct-constructor (hash-ref constructor-lookup (first place-message))])
    (apply struct-constructor (rest place-message))))
