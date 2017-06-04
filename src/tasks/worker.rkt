;;
;; worker.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This procedure defines a worker "place". A place is a separate instance of the
;; Racket runtime which is used to support SMP parallelism throughout the server.
;;

#lang racket

;; -- Requires --

(require "../tasks/task.rkt")
(require "../tasks/task-serialization.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; -- Worker Management --

  ;; Creates a new worker.
  ;; - identifier: the identifier of the worker.
  ;; - output-channel: a channel to use to send messages to the manager.
  [make-worker (->i ([identifier worker-identifier?]
                     [place-to-manager-channel place-channel?])
                    [result worker?])]

  ;; Predicate returning #t if the argument is a worker object.
  [worker? (-> any/c boolean?)]

  ;; Returns the identifier for the specified worker.
  [worker-identifier (-> worker? worker-identifier?)]

  ;; Enqueues a task with the worker.
  [worker-enqueue-task (-> worker? gen:task-handle? void?)]

  ;; Terminates a worker. If synchronous is #t, this method will block until
  ;; (worker-terminated-evt) is ready for synchronization.
  [worker-terminate (->i ([worker worker?])
                         (#:synchronous [synchronous boolean?])
                         [result void?])]

  ;; Returns an event which is ready for synchronization when the place for the
  ;; specified worker is no longer running. The synchronization result is the
  ;; worker object itself.
  [worker-terminated-evt (-> worker? evt?)]

  ;; -- Utility --

  ;; Predicate returning #t if the argument is a valid worker identifier.
  [worker-identifier? (-> any/c boolean?)]))

;; -- Parameters --

(define current-worker-identifier
  (make-parameter #f))

(define current-worker-pch-to-manager
  (make-parameter #f))

(define current-worker-pch-from-manager
  (make-parameter #f))

;; -- Structs --

(struct worker (identifier place))

;; -- Public Procedures (Worker Management) --

(define (make-worker identifier pch-to-manager)
  ;; Create the place
  (let ([pl (place bootstrap-pch
              ;; Set place configuration
              (parameterize*
                  ([current-worker-pch-from-manager bootstrap-pch]
                   [current-worker-pch-to-manager (place-channel-get bootstrap-pch)]
                   [current-worker-identifier (place-channel-get bootstrap-pch)])
                ;; Run the main worker procedure
                (worker-main)))])
    ;; Notify place of identifier and channel.
    (place-channel-put pl pch-to-manager)
    (place-channel-put pl identifier)
    ;; Return struct
    (worker identifier pl)))

(define (worker-enqueue-task worker task-handle)
  (worker-put worker
              (list
               'enqueue-task
               (gen:task-handle->place-message task-handle))))

(define (worker-terminate worker
                          #:immediately [immediately #t]
                          #:synchronous [synchronous #t])
  (worker-put worker (list 'terminate immediately))
  (when synchronous
    (sync (worker-terminated-evt worker)))
  (void))

(define (worker-terminated-evt worker)
  (wrap-evt
   (place-dead-evt (worker-place worker))
   (λ (evt) worker)))

;; -- Public Procedures (Utility) --

(define worker-identifier? string?)

;; -- Private Procedures --

(define (worker-put worker message)
  (place-channel-put (worker-place worker) message))

(define (worker-main)
  (let loop ([tasks (set)] [terminating #f])
    (sync
     ;; Event from manager?
     (handle-evt
      (current-worker-pch-from-manager)
      (match-lambda
        ;; Received command (terminate)
        [(list 'terminate (? boolean? immediately))
         ;; If we're terminating immediately, cancel all tasks
         (when immediately
           (for ([task (in-set tasks)])
             (gen:task-cancel task #:synchronous #f)))
         ;; If there's still tasks, loop until they're done
         (when (not (set-empty? tasks))
           (loop tasks #t))]
        ;; Received command (enqueue-task)
        [(list 'enqueue-task task-pm)
         (let* ([task-handle (place-message->gen:task-handle task-pm)]
                [task (gen:task-handle->gen:task task-handle)])
           (gen:task-start task)
           (loop (set-add tasks task) terminating))]
        ;; Unrecognized message - log and continue looping
        [unrecognized-message
         (loop tasks terminating)]))
     ;; Completed task?
     (handle-evt
      (apply choice-evt (set-map tasks gen:task-completed-evt))
      (λ (task)
        ;; Notify manager that task is completed
        (place-channel-put
         (current-worker-pch-to-manager)
         (list
          'task-completed
          (current-worker-identifier)
          (gen:task-identifier task)))
        ;; Loop only as long as either we're not terminating or there are tasks left
        (let ([updated-tasks (set-remove tasks task)])
          (when (not (and terminating (set-empty? updated-tasks)))
            (loop updated-tasks terminating))))))))

;; -- Tests --

(module+ test

  ;; -- Requires --

  (require rackunit)
  (require "../tasks/example-task.rkt")

  ;; -- Test Cases --

  ;; Worker Lifecycle
  (let*-values ([(worker-identifier) "Test Worker"]
                [(our-endpoint their-endpoint) (place-channel)]
                [(worker) (make-worker worker-identifier their-endpoint)])
    ;; Verify the worker is alive
    (check-false (sync/timeout 0.5 (worker-terminated-evt worker)))
    ;; Verify the worker dies when terminated
    (worker-terminate worker #:synchronous #f)
    (check-equal? (sync/timeout 5.0 (worker-terminated-evt worker)) worker))

  ;; Worker Task Management
  (let*-values ([(worker-identifier) "Test Worker"]
                [(task-identifier) "Example Task"]
                [(our-endpoint their-endpoint) (place-channel)]
                [(worker) (make-worker worker-identifier their-endpoint)])
    ;; Enqueue a task with the worker
    (worker-enqueue-task
     worker
     (example-task-handle task-identifier "This Is A Message" #f))
    ;; Wait to get back confirmation that task was completed
    (let ([response (sync/timeout 5.0 our-endpoint)])
      (check-equal? response (list 'task-completed worker-identifier task-identifier)))
    (worker-terminate worker))

  ;; Worker Task Cancellation - Immediate
  (let*-values ([(worker-identifier) "Test Worker"]
                [(task-identifier) "Example Task"]
                [(our-endpoint their-endpoint) (place-channel)]
                [(worker) (make-worker worker-identifier their-endpoint)])
    ;; Enqueue task
    (worker-enqueue-task
     worker
     (example-task-handle task-identifier "Looping!" #t))
    (check-false (sync/timeout 2.0 (worker-terminated-evt worker)))
    (worker-terminate worker #:immediately #t #:synchronous #f)
    (let ([response (sync/timeout 0.5 our-endpoint)])
      (check-equal? response (list 'task-completed worker-identifier task-identifier)))
    (check-equal? (sync/timeout 0.5 (worker-terminated-evt worker)) worker)))
