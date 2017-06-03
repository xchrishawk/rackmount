;;
;; manager-state.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines a manager state object, which is used to keep track of the
;; workers and tasks currently being managed by the manager.
;;
;; The general structure is:
;;
;; - manager-state
;;   - worker-lookup (hash of worker-identifier? -> worker-state)
;; - worker-state
;;   - worker (worker?)
;;   - task-lookup (hash of gen:task-identifer? -> gen:task-handle)
;;

#lang racket

;; -- Requires --

(require "../worker/task.rkt")
(require "../worker/worker.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Creates a new manager state object.
  [rename make-manager-state manager-state (-> (listof worker?) manager-state?)]

  ;; Selects the appropriate worker for the next ask.
  [manager-state-select-worker (-> manager-state? worker?)]

  ;; Gets the worker with the specified identifier.
  [manager-state-worker (-> manager-state? worker-identifier? worker?)]

  ;; Gets a list of all workers.
  [manager-state-workers (-> manager-state? (listof worker?))]

  ;; Gets a list of the get events for all workers.
  [manager-state-worker-get-evts (-> manager-state? (listof evt?))]

  ;; Adds a new task handle to the specified worker.
  [manager-state-add-task-handle (-> manager-state?
                                     worker-identifier?
                                     gen:task-handle?
                                     manager-state?)]

  ;; Removes a task handle from the specified worker.
  [manager-state-remove-task-handle (-> manager-state?
                                        worker-identifier?
                                        gen:task-identifier?
                                        manager-state?)]

  ;; Gets the task handle with the specified identifier from the specified worker.
  [manager-state-get-task-handle (-> manager-state?
                                     worker-identifier?
                                     gen:task-identifier?
                                     gen:task-handle?)]))

;; -- Structs --

(struct manager-state (worker-lookup))

(struct worker-state (worker task-lookup))

;; -- Public Procedures --

(define (make-manager-state worker-list)
  (manager-state
   (for/hash ([worker (in-list worker-list)])
     (values (worker-identifier worker)
             (worker-state worker (hash))))))

(define (manager-state-select-worker state)
  ;; For now, just pick one at random. TODO: better selection
  (let ([worker-states (hash-values (manager-state-worker-lookup state))])
    (worker-state-worker (list-ref worker-states (random (length worker-states))))))

(define (manager-state-worker state worker-identifier)
  (let* ([worker-lookup (manager-state-worker-lookup state)]
         [worker-state (hash-ref worker-lookup worker-identifier)])
    (worker-state-worker worker-state)))

(define (manager-state-workers state)
  (map
   worker-state-worker
   (hash-values (manager-state-worker-lookup state))))

(define (manager-state-worker-get-evts state)
  (map worker-get-evt (manager-state-workers state)))

(define (manager-state-add-task-handle state worker-identifier task-handle)
  (let* ([old-worker-lookup (manager-state-worker-lookup state)]
         [old-worker-state (hash-ref old-worker-lookup worker-identifier)]
         [new-worker-state (worker-state-add-task-handle old-worker-state task-handle)]
         [new-worker-lookup (hash-set old-worker-lookup worker-identifier new-worker-state)])
    (struct-copy manager-state state [worker-lookup new-worker-lookup])))

(define (manager-state-remove-task-handle state worker-identifier task-handle-identifier)
  (let* ([old-worker-lookup (manager-state-worker-lookup state)]
         [old-worker-state (hash-ref old-worker-lookup worker-identifier)]
         [new-worker-state (worker-state-remove-task-handle old-worker-state task-handle-identifier)]
         [new-worker-lookup (hash-set old-worker-lookup worker-identifier new-worker-state)])
    (struct-copy manager-state state [worker-lookup new-worker-lookup])))

(define (manager-state-get-task-handle state worker-identifier task-handle-identifier)
  (let* ([worker-lookup (manager-state-worker-lookup state)]
         [worker-state (hash-ref worker-lookup worker-identifier)])
    (worker-state-get-task-handle worker-state task-handle-identifier)))

;; -- Private Procedures --

(define (worker-state-add-task-handle state task-handle)
  (let* ([old-task-lookup (worker-state-task-lookup state)]
         [new-task-lookup (hash-set old-task-lookup (gen:task-handle-identifier task-handle) task-handle)])
    (struct-copy worker-state state [task-lookup new-task-lookup])))

(define (worker-state-remove-task-handle state task-handle-identifier)
  (let* ([old-task-lookup (worker-state-task-lookup state)]
         [new-task-lookup (hash-remove old-task-lookup task-handle-identifier)])
    (struct-copy worker-state state [task-lookup new-task-lookup])))

(define (worker-state-get-task-handle state task-handle-identifier)
  (let* ([task-lookup (worker-state-task-lookup state)])
    (hash-ref task-lookup task-handle-identifier)))
