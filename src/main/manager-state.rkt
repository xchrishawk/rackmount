;;
;; manager-state.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines types and procedures to assist the manager with tracking
;; the state of its worker places.
;;

#lang racket

;; -- Requires --

(require "../worker/task.rkt")
(require "../worker/worker.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Creates a new manager state object.
  [manager-state (-> (listof worker?) opaque-manager-state?)]

  ;; Gets the worker with the specified identifier.
  [manager-state-get-worker (-> opaque-manager-state? worker-identifier? worker?)]

  ;; Adds a new task handle to the specified worker.
  [manager-state-add-task-handle (-> opaque-manager-state? worker-identifier? gen:task-handle? opaque-manager-state?)]

  ;; Removes a task handle from the specified worker.
  [manager-state-remove-task-handle (-> opaque-manager-state? worker-identifier? gen:task-identifier? opaque-manager-state?)]

  ;; Gets the task handle with the specified identifier from the specified worker.
  [manager-state-get-task-handle (-> opaque-manager-state? worker-identifier? gen:task-identifier? gen:task-handle?)]))

;; -- Structs --

(struct opaque-manager-state (worker-lookup))

(struct opaque-worker-state (worker task-lookup))

;; -- Public Procedures --

(define (manager-state worker-list)
  (opaque-manager-state
   (for/hash ([worker (in-list worker-list)])
     (values (worker-identifier worker)
             (opaque-worker-state worker (hash))))))

(define (manager-state-get-worker manager-state worker-identifier)
  (let* ([worker-lookup (opaque-manager-state-worker-lookup manager-state)]
         [worker-state (hash-ref worker-lookup worker-identifier)])
    (opaque-worker-state-worker worker-state)))

(define (manager-state-add-task-handle manager-state worker-identifier task-handle)
  (let* ([old-worker-lookup (opaque-manager-state-worker-lookup manager-state)]
         [old-worker-state (hash-ref old-worker-lookup worker-identifier)]
         [new-worker-state (worker-state-add-task-handle old-worker-state task-handle)]
         [new-worker-lookup (hash-set old-worker-lookup worker-identifier new-worker-state)])
    (struct-copy opaque-manager-state manager-state [worker-lookup new-worker-lookup])))

(define (manager-state-remove-task-handle manager-state worker-identifier task-handle-identifier)
  (let* ([old-worker-lookup (opaque-manager-state-worker-lookup manager-state)]
         [old-worker-state (hash-ref old-worker-lookup worker-identifier)]
         [new-worker-state (worker-state-remove-task-handle old-worker-state task-handle-identifier)]
         [new-worker-lookup (hash-set old-worker-lookup worker-identifier new-worker-state)])
    (struct-copy opaque-manager-state manager-state [worker-lookup new-worker-lookup])))

(define (manager-state-get-task-handle manager-state worker-identifier task-handle-identifier)
  (let* ([worker-lookup (opaque-manager-state-worker-lookup manager-state)]
         [worker-state (hash-ref worker-lookup worker-identifier)])
    (worker-state-get-task-handle worker-state task-handle-identifier)))

;; -- Private Procedures --

(define (worker-state-add-task-handle worker-state task-handle)
  (let* ([old-task-lookup (opaque-worker-state-task-lookup worker-state)]
         [new-task-lookup (hash-set old-task-lookup (gen:task-handle-identifier task-handle) task-handle)])
    (struct-copy opaque-worker-state worker-state [task-lookup new-task-lookup])))

(define (worker-state-remove-task-handle worker-state task-handle-identifier)
  (let* ([old-task-lookup (opaque-worker-state-task-lookup worker-state)]
         [new-task-lookup (hash-remove old-task-lookup task-handle-identifier)])
    (struct-copy opaque-worker-state worker-state [task-lookup new-task-lookup])))

(define (worker-state-get-task-handle worker-state task-handle-identifier)
  (let* ([task-lookup (opaque-worker-state-task-lookup worker-state)])
    (hash-ref task-lookup task-handle-identifier)))
