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

  ;; Struct representing a client task.
  [struct client-task ([identifier client-task-identifier?]
                       [input-port input-port?]
                       [output-port output-port?])]

  ;; Predicate returning #t if the argument is a valid client task identifier.
  [client-task-identifier? (-> any/c boolean?)]

  ;; Returns a generator building a sequence of client identifiers.
  [client-task-identifier-generator (-> (-> client-task-identifier?))]

  ;; Predicate returning #t if the argument is a valid list representation for a
  ;; client task struct.
  [client-task-list? (-> any/c boolean?)]

  ;; Converts a list representation to a client-task struct.
  [list->client-task (-> client-task-list? client-task?)]))

;; -- Structs --

(struct client-task (identifier
                     input-port
                     output-port)
  #:methods gen:task
  [(define (gen:task-identifier task)
     (client-task-identifier task))
   (define (gen:task->list task)
     (list 'client-task
           (client-task-identifier task)
           (client-task-input-port task)
           (client-task-output-port task)))])


;; -- Public Procedures --

(define client-task-identifier?
  string?)

(define (client-task-identifier-generator)
  (let ([naturals (sequence->generator (in-naturals))])
    (λ ()
      (format "Client ~A" (naturals)))))

(define client-task-list?
  (list/c 'client-task string? input-port? output-port?))

(define (list->client-task lst)
  (apply client-task (rest lst)))
