;;
;; worker.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This procedure defines a worker "place". A place is a separate instance of the
;; Racket runtime which is used to support SMP parallelism throughout the server.
;;

#lang racket

;; -- Requires --

(require "../main/configuration.rkt")
(require "../tasks/task.rkt")
(require "../tasks/task-serialization.rkt")
(require "../util/exceptions.rkt")
(require "../util/logging.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; -- Worker Management --

  ;; Creates a new worker.
  ;; - identifier: the identifier of the worker.
  ;; - output-channel: a channel to use to send messages to the manager.
  [rename make-worker worker (-> worker-identifier? worker?)]

  ;; Predicate returning #t if the argument is a worker object.
  [worker? (-> any/c boolean?)]

  ;; Returns the identifier for the specified worker.
  [worker-identifier (-> worker? worker-identifier?)]

  ;; Enqueues a task with the worker.
  [worker-enqueue (-> worker? gen:task-handle? void?)]

  ;; Terminates a worker. If synchronous is #t, this method will block until
  ;; (worker-terminated-evt) is ready for synchronization.
  [worker-terminate (->i ([worker worker?])
                         (#:immediately [immediately boolean?]
                          #:synchronous [synchronous boolean?])
                         [result void?])]

  ;; Returns an event which is ready for synchronization when a message can be
  ;; received from the worker. The synchronization result is the received message.
  [worker-get-evt (-> worker? evt?)]

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

(struct worker (identifier		; identifier of the worker
                place			; worker place handle
                pch-to-manager		; place channel to use to *receive* data
                pch-from-manager))	; place channel to use to *send* data

;; -- Public Procedures (Worker Management) --

(define (make-worker identifier)
  ;; Create the place
  (let-values ([(our-pch-to-manager their-pch-to-manager) (place-channel)]
               [(our-pch-from-manager their-pch-from-manager) (place-channel)]
               [(pl) (place bootstrap-pch
                       ;; Set place configuration
                       (parameterize*
                           ([config-minimum-log-event-level (place-channel-get bootstrap-pch)]
                            [config-server-name (place-channel-get bootstrap-pch)]
                            [config-session-timeout (place-channel-get bootstrap-pch)]
                            [config-working-dir (place-channel-get bootstrap-pch)]
                            [current-worker-pch-to-manager (place-channel-get bootstrap-pch)]
                            [current-worker-pch-from-manager (place-channel-get bootstrap-pch)]
                            [current-worker-identifier (place-channel-get bootstrap-pch)])
                         ;; Notify startup is complete
                         (place-channel-put bootstrap-pch 'alive)
                         ;; Run the main worker procedure
                         (worker-main)))])
    ;; Configure the place
    (place-channel-put pl (config-minimum-log-event-level))
    (place-channel-put pl (config-server-name))
    (place-channel-put pl (config-session-timeout))
    (place-channel-put pl (config-working-dir))
    (place-channel-put pl their-pch-to-manager)
    (place-channel-put pl their-pch-from-manager)
    (place-channel-put pl identifier)
    ;; Wait for notification that place is alive
    (place-channel-get pl)
    ;; Return struct
    (worker identifier pl our-pch-to-manager our-pch-from-manager)))

(define (worker-enqueue worker task-handle)
  (place-channel-put
   (worker-pch-from-manager worker)
   (list 'enqueue-task (gen:task-handle->place-message task-handle))))

(define (worker-terminate worker
                          #:immediately [immediately #t]
                          #:synchronous [synchronous #t])
  (place-channel-put
   (worker-pch-from-manager worker)
   (list 'terminate immediately))
  (when synchronous
    (sync (worker-terminated-evt worker)))
  (void))

(define (worker-get-evt worker)
  (wrap-evt (worker-pch-to-manager worker) identity))

(define (worker-terminated-evt worker)
  (wrap-evt
   (place-dead-evt (worker-place worker))
   (λ (evt) worker)))

;; -- Public Procedures (Utility) --

(define worker-identifier? string?)

;; -- Private Procedures --

(define (worker-main)

  ;; Local logging procedures
  (define-local-log worker "Worker" #:identifier (current-worker-identifier))

  ;; Helper procedure - notify manager that task is complete
  (define (notify-task-completed task-identifier)
    (place-channel-put
     (current-worker-pch-to-manager)
     (list
      'task-completed
      (current-worker-identifier)
      task-identifier)))

  (worker-log-trace "Worker running.")

  ;; Main loop - wait for an event
  (let loop ([tasks (set)] [terminating #f])
    (sync

     ;; Event from manager?
     (handle-evt
      (current-worker-pch-from-manager)
      (match-lambda
        ;; Received command (terminate)
        [(list 'terminate (? boolean? immediately))
         ;; If we're terminating immediately, cancel all tasks
         (worker-log-trace "Received terminate command (immediately = ~A)" immediately)
         (when immediately
           (for ([task (in-set tasks)])
             (gen:task-cancel task #:synchronous #f)))
         ;; If there's still tasks, loop until they're done
         (when (not (set-empty? tasks))
           (loop tasks #t))]
        ;; Received command (enqueue-task)
        [(list 'enqueue-task task-pm)
         (let ([task-handle (place-message->gen:task-handle task-pm)])
           (if (not terminating)
               ;; We're not already terminating - go ahead and start the task
               (begin
                 (worker-log-trace
                  "Received task (~A), starting it now. ~A tasks currently active on this worker."
                  (gen:task-handle-identifier task-handle)
                  (add1 (set-count tasks)))
                 (let ([task (gen:task-handle->gen:task task-handle)])
                   (gen:task-start task)
                   (loop (set-add tasks task) terminating)))
               ;; We are terminating - reject the task
               (begin
                 (worker-log-trace
                  "Received task (~A), but worker is terminating, so rejecting it."
                  (gen:task-handle-identifier task-handle))
                 (notify-task-completed (gen:task-handle-identifier task-handle))
                 (loop tasks terminating))))]
        ;; Unrecognized message - log and continue looping
        [bad-message (raise-bad-message-error bad-message)]))

     ;; Completed task?
     (handle-evt
      (apply choice-evt (set-map tasks gen:task-completed-evt))
      (λ (task)
        (worker-log-trace
         "Task (~A) completed, notifying manager. ~A tasks currently active on this worker."
         (gen:task-identifier task)
         (sub1 (set-count tasks)))
        ;; Notify manager that task is completed
        (notify-task-completed (gen:task-identifier task))
        ;; Loop only as long as either we're not terminating or there are tasks left
        (let ([updated-tasks (set-remove tasks task)])
          (when (not (and terminating (set-empty? updated-tasks)))
            (loop updated-tasks terminating)))))))

  (worker-log-trace "Worker terminated."))

;; -- Tests --

(module+ test

  ;; -- Requires --

  (require rackunit)
  (require "../tasks/example-task.rkt")

  ;; -- Helpers --

  (define worker-id "Test Worker")
  (define task-id "Example Task")

  (define (check-worker-alive worker timeout)
    (check-false (sync/timeout timeout (worker-terminated-evt worker))))

  (define (check-worker-dead worker timeout)
    (check-equal? (sync/timeout timeout (worker-terminated-evt worker)) worker))

  (define (check-task-completed worker task-identifier timeout)
    (let ([result (sync/timeout timeout (worker-get-evt worker))])
      (check-equal? result (list 'task-completed
                                 (worker-identifier worker)
                                 task-identifier))))

  (define (check-task-not-completed worker timeout)
    (check-false (sync/timeout timeout (worker-get-evt worker))))

  ;; -- Test Cases --

  (parameterize ([config-minimum-log-event-level 'critical])

    ;; Worker Lifecycle
    (let ([worker (make-worker worker-id)])
      (check-worker-alive worker 1.0)
      (worker-terminate worker #:synchronous #f)
      (check-worker-dead worker 1.0))

    ;; Worker Task Management
    (let ([worker (make-worker worker-id)])
      ;; Enqueue a task with the worker
      (worker-enqueue worker (example-task-handle task-id 3.0))
      ;; Verify we don't get completion back before the task has completed
      (check-task-not-completed worker 2.5)
      ;; Verify we *do* get completion back after the task has completed
      (check-task-completed worker task-id 1.0)
      (worker-terminate worker))

    ;; Worker Task Cancellation - Immediate
    (let ([worker (make-worker worker-id)])
      ;; Enqueue task with indefinite duration
      (worker-enqueue worker (example-task-handle task-id #f))
      ;; Verify worker continues to run
      (check-worker-alive worker 2.0)
      ;; Terminate the worker immediately
      (worker-terminate worker #:immediately #t #:synchronous #f)
      ;; Verify we get back confirmation that the task was cancelled
      (check-task-completed worker task-id 0.5)
      ;; Verify that the worker terminates
      (check-worker-dead worker 0.5))

    ;; Worker Task Cancellation - Not Immediate
    (let ([worker (make-worker worker-id)])
      ;; Enqueue task with 3 second duration
      (worker-enqueue worker (example-task-handle task-id 3.0))
      ;; Terminate worker with immediate set to #f
      (worker-terminate worker #:immediately #f #:synchronous #f)
      ;; Verify worker continues to run in the meantime
      (check-worker-alive worker 2.5)
      ;; Verify we get back confirmation the the task completed
      (check-task-completed worker task-id 1.0)
      ;; Verify that the worker is now terminated
      (check-worker-dead worker 0.5))

    ;; Worker Task Rejection
    (let ([worker (make-worker worker-id)])
      ;; Enqueue task with 3 second duration
      (worker-enqueue worker (example-task-handle "Accepted Task" 3.0))
      ;; Terminate worker with immediate set to #f
      (worker-terminate worker #:immediately #f #:synchronous #f)
      ;; Enqueue new task which should be rejected
      (worker-enqueue worker (example-task-handle "Rejected Task" #f))
      ;; Verify that we immediately get back a completion for the rejected task
      (check-task-completed worker "Rejected Task" 0.5)
      ;; Verify that we get back a completion for the accepted task once it dies naturally
      (check-task-completed worker "Accepted Task" 3.5)
      ;; Verify worker is now terminated
      (check-worker-dead worker 0.5))))
