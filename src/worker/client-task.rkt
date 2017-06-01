;;
;; client-task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures for managing client tasks, both in
;; the manager thread and in worker places.
;;

#lang racket

;; -- Requires --

(require racket/generator)
(require "../worker/task.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; -- Identifiers --

  ;; Predicate returning #t if the argument is a valid client task identifier.
  [client-task-identifier? (-> any/c boolean?)]

  ;; Returns a generator building a sequence of client identifiers.
  [client-task-identifier-generator (-> (-> client-task-identifier?))]

  ;; -- Task Handle --

  ;; Struct representing a client task.
  [struct client-task-handle ([identifier client-task-identifier?]
                              [input-port input-port?]
                              [output-port output-port?])]))

;; -- Structs --

(struct client-task-handle (identifier
                            input-port
                            output-port)
  #:methods gen:task-handle
  [(define (gen:task-handle-identifier task-handle)
     (client-task-handle-identifier task-handle))

   (define (gen:task-handle-close task-handle)
     (close-input-port (client-task-handle-input-port task-handle))
     (close-output-port (client-task-handle-output-port task-handle)))

   (define (gen:task-handle->list task-handle)
     (list 'client-task
           (client-task-handle-identifier task-handle)
           (client-task-handle-input-port task-handle)
           (client-task-handle-output-port task-handle)))])


;; -- Public Procedures --

(define client-task-identifier?
  string?)

(define (client-task-identifier-generator)
  (let ([naturals (sequence->generator (in-naturals))])
    (λ ()
      (format "Client ~A" (naturals)))))
