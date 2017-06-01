;;
;; task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Class defining a generic interface for tasks which may be queued with one of
;; the manager's worker places.
;;

#lang racket

;; -- Requires --

(require racket/generic)

;; -- Provides --

(provide

 ;; Generic interface for queueable tasks.
 gen:task

 (contract-out

  ;; Predicate returning #t if the argument implements the gen:task interface.
  [rename task? gen:task? (-> any/c boolean?)]

  ;; Returns an identifier for this task.
  [gen:task-identifier (-> task? any/c)]

  ;; Converts a task to a list which can be transmitted over a place channel.
  [gen:task->list (-> task? (listof place-message-allowed?))]))

;; -- Public Procedures --

(define-generics task
  (gen:task-identifier task)
  (gen:task->list task))
