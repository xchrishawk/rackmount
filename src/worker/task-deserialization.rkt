;;
;; task-deserialization.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Module defining a function to deserialize gen:task objects from their list
;; representations. This needs to go in its own module to resolve a circular
;; dependency between task.rkt and the modules which define individual tasks.
;;

#lang racket

;; -- Requires --

(require "../worker/client-task.rkt")
(require "../worker/task.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Predicate returning #t if the specified argument is a list representing
  ;; a known task type.
  [gen:task-list? (-> any/c boolean?)]

  ;; Converts a list representation to a gen:task object.
  [list->gen:task (-> gen:task-list? gen:task?)]))

;; -- Procedures --

(define (gen:task-list? x) #f)

(define (list->gen:task lst)
  (error "TODO"))
