;;
;; client-task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures for managing client tasks, both in
;; the manager thread and in worker places. It does *not* define the actual logic
;; used to communicate with the client - that is done in client.rkt.
;;

#lang racket

;; -- Requires --

(require racket/generator)
(require "../util/misc.rkt")
(require "../worker/client.rkt")
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
                              [output-port output-port?]
                              [working-dir path-string?]
                              [timeout positive?])]

  ;; -- Serialization --

  ;; Predicate returning #t if the argument is a valid list for a client task.
  [client-task-list? (-> any/c boolean?)]

  ;; Converts a client task list to a client task.
  [list->client-task (-> client-task-list? gen:task?)]))

;; -- Structs --

(struct client-task-handle (identifier
                            input-port
                            output-port
                            working-dir
                            timeout)
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
           (client-task-handle-output-port task-handle)
           (client-task-handle-working-dir task-handle)
           (client-task-handle-timeout task-handle)))])

(struct client-task (identifier
                     input-port
                     output-port
                     working-dir
                     timeout
                     thread)
  #:mutable
  #:methods gen:task
  [(define (gen:task-identifier task)
     (client-task-identifier task))
   (define (gen:task-start task)
     (let ([thd (thread-start
                 (client-proc (client-task-identifier task)
                              (client-task-input-port task)
                              (client-task-output-port task)
                              (client-task-working-dir task)
                              (client-task-timeout task)))])
       (set-client-task-thread! task thd)))
   (define (gen:task-cancel task)
     (error "TODO"))
   (define (gen:task-completed-evt task)
     (wrap-evt (client-task-thread task) (λ (evt) task)))])

;; -- Public Procedures --

(define client-task-identifier?
  string?)

(define (client-task-identifier-generator)
  (let ([naturals (sequence->generator (in-naturals))])
    (λ ()
      (format "Client ~A" (naturals)))))

(define client-task-list?
  (list/c 'client-task
          client-task-identifier?
          input-port?
          output-port?
          path-string?
          positive?))

(define (list->client-task lst)
  (apply client-task (flatten (list (rest lst) #f))))
