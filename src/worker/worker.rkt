;;
;; worker.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the main entry point for worker places. A "place" in Racket
;; is a separate instance of the runtime environment that can run in parallel with
;; the main instance. They are used to provide parallelism in the server.
;;

#lang racket

;; -- Requires --

(require "../worker/task.rkt")
(require "../worker/task-deserialization.rkt")
(require "../util/logging.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Predicate returning #t if the argument is a worker object.
  [rename opaque-worker? worker? (-> any/c boolean?)]

  ;; Predicate returning #t if the argument is a valid worker identifier.
  [worker-identifier? (-> any/c boolean?)]

  ;; Starts a new worker with the specified identifier.
  [worker-start (-> worker-identifier? opaque-worker?)]

  ;; Stops the specified worker.
  [worker-stop (-> opaque-worker? void?)]

  ;; Returns the identifier of the specified worker.
  [worker-identifier (-> opaque-worker? worker-identifier?)]

  ;; Sends a message to the specified worker.
  [worker-put (-> opaque-worker? place-message-allowed? void?)]

  ;; Returns an event which is ready for synchronization when there is a message
  ;; available from the specified worker. The synchronization result is a list
  ;; consisting of the symbol 'worker-message, followed by the worker's identifier,
  ;; followed by the message itself.
  [worker-get-evt (-> opaque-worker? evt?)]))

;; -- Types --

(struct opaque-worker (identifier place channel))

;; -- Public Procedures --

(define worker-identifier?
  string?)

(define (worker-start identifier)
  (let-values ([(worker-place) (make-worker-place)]
               [(our-channel their-channel) (place-channel)])
    (place-channel-put worker-place identifier)
    (place-channel-put worker-place their-channel)
    (opaque-worker identifier worker-place our-channel)))

(define (worker-stop worker)
  (place-channel-put (opaque-worker-channel worker) 'shutdown)
  (sync (place-dead-evt (opaque-worker-place worker)))
  (void))

(define (worker-identifier worker)
  (opaque-worker-identifier worker))

(define (worker-put worker message)
  (place-channel-put (opaque-worker-channel worker) message))

(define (worker-get-evt worker)
  (let ([identifier (opaque-worker-identifier worker)])
    (wrap-evt
     (opaque-worker-channel worker)
     (λ (evt)
       (list 'worker-message identifier evt)))))

;; -- Private Procedures --

;; Launches and returns a new place.
(define (make-worker-place)
  ;; Launch the place
  (place bootstrap-channel
    ;; Get initial configuration from the bootstrap channel
    (let ([identifier (place-channel-get bootstrap-channel)]
          [channel (place-channel-get bootstrap-channel)])
      (worker-log-trace identifier "Worker launched.")
      ;; Enter the main loop
      (let loop ([tasks (set)])
        ;; Wait for an event
        (let ([evt (apply sync
                          channel
                          (log-event-dequeue-evt)
                          (set-map tasks gen:task-completed-evt))])
          (match evt
            ;; Log event enqueued - forward to manager so it can be enqueued in the
            ;; main place's log queue.
            ;;
            ;; NOTE - It was originally intended that log-event would be a prefab
            ;; struct to allow sending it directly across the place channel.
            ;; However, prefab structs with contracts fail to send across place
            ;; channels (see https://stackoverflow.com/q/44274675/434245). As a
            ;; workaround, we just wrap the event into a list and send it that way
            ;; (rather than removing the contracts on the struct).
            [(? log-event? log-event)
             (place-channel-put channel (log-event->list log-event))
             (loop tasks)]
            ;; Task request from manager
            [(? gen:task-list? task-list)
             (let ([task (list->gen:task task-list)])
               (worker-log-trace identifier
                                 "Received task with identifier ~A, starting it..."
                                 (gen:task-identifier task))
               (gen:task-start task)
               (loop (set-add tasks task)))]
            ;; Task completed - notify manager and remove from task list
            [(? (λ (x) (set-member? tasks x)) task)
             (place-channel-put channel (list 'task-completed (gen:task-identifier task)))
             (loop (set-remove tasks task))]
            ;; Shutdown command - exit loop, returning remaining task list
            ['shutdown tasks]
            ;; Unknown event? Log and continue
            [else
             (worker-log-error identifier "Received unknown event (~A). Ignoring..." evt)
             (loop)])))
      (worker-log-trace identifier "Worker terminated."))))

;; Local logging functions.
(define-local-log worker "Worker" #:with-identifier #t)
