;;
;; worker.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines worker places, which are used to implement parallelism
;; for the server. Each worker represents a Racket "place", which is a separate
;; instance of the Racket runtime environment that can run fully in parallel
;; with other places.
;;

#lang racket

;; -- Requires --

(require syntax/location)

;; -- Provides --

(provide
 (contract-out
  ;; Create a new worker with the specified identifier.
  [make-worker (-> string? opaque-worker?)]
  ;; Returns the identifier of a worker.
  [worker-identifier (-> opaque-worker? string?)]
  ;; Sends a job to a worker.
  [worker-send-job (-> opaque-worker? any/c void?)]
  ;; Returns the number of active jobs the worker has.
  [worker-job-count (-> opaque-worker? exact-nonnegative-integer?)]
  ;; Terminates the specified worker.
  [worker-terminate (-> opaque-worker? exact-nonnegative-integer?)]
  ;; Makes N workers with auto-generated identifiers.
  [make-workers (-> exact-positive-integer? (listof opaque-worker?))]
  ;; Sends a job to the worker with the lowest number of active jobs.
  [workers-send-job (-> (listof opaque-worker?) any/c void?)]
  ;; Terminates all of the specified workers.
  [workers-terminate (-> (listof opaque-worker?) void?)]))

;; -- Structs --

;; Opaque struct containing worker data.
(struct opaque-worker (identifier place channel))

;; -- Public Procedures --

(define (make-worker identifier)
  (let* ([worker-place (dynamic-place (quote-module-path worker-module) 'start)]
         [worker-channel (place-channel-get worker-place)])
    (place-channel-put worker-place identifier)
    (opaque-worker identifier worker-place worker-channel)))

(define (worker-identifier worker)
  (opaque-worker-identifier worker))

(define (worker-send-job worker job)
  (place-channel-put (opaque-worker-channel worker) job))

(define (worker-job-count worker)
  (place-channel-put/get (opaque-worker-channel worker) 'job-count))

(define (worker-terminate worker)
  (place-channel-put (opaque-worker-channel worker) 'terminate)
  (place-wait (opaque-worker-place worker)))

(define (make-workers count)
  (for/list ([n (in-range count)])
    (let ([identifier (format "Worker ~A" n)])
      (make-worker identifier))))

(define (workers-send-job workers job)
  (let ([worker (worker-with-lowest-job-count workers)])
    (worker-send-job worker job)))

(define (workers-terminate workers)
  (for ([worker (in-list workers)])
    (worker-terminate worker)))

;; -- Private Procedures --

;; Returns the worker from the list with the lowest job count.
(define (worker-with-lowest-job-count workers)
  (let loop ([workers workers] [current-worker #f] [current-count #f])
    (if (null? workers)
        current-worker
        (let* ([this-worker (first workers)]
               [this-count (worker-job-count this-worker)])
          (cond
            ;; If worker has zero jobs, no point in checking any other worker
            [(zero? this-count) this-worker]
            ;; This worker is the lowest
            [(or (not current-worker)
                 (< this-count current-count))
             (loop (rest workers) this-worker this-count)]
            ;; This worker is not the lowest
            [else (loop (rest workers) current-worker current-count)])))))

;; -- Worker Module --

(module worker-module racket

  ;; -- Requires --

  (require "log.rkt")

  ;; -- Provides --

  (provide
   (contract-out
    [start (-> place-channel? any)]))

  ;; -- Parameters --

  (define worker-identifier (make-parameter #f))
  (define worker-channel (make-parameter #f))

  ;; -- Startup Procedure --

  ;; Startup procedure - exchanges identifier and channel information with caller
  (define (start bootstrap-channel)
    (let*-values ([(our-channel their-channel) (place-channel)]
                  [(identifier) (place-channel-put/get bootstrap-channel their-channel)])
      (parameterize ([worker-identifier identifier]
                     [worker-channel our-channel])
        (main))))

  ;; -- Private Procedures --

  (define (main)
    (worker-log "Worker launched, waiting for jobs...")
    ;; Enter the main loop for this worker
    (let loop ([job-threads (set)])
      ;; Wait for an event to occur
      (let* ([syncable-evts (list (worker-channel)		; message from controller
                                  (set->list job-threads))]     ; job thread termination
             [next-evt (apply sync (flatten syncable-evts))])
        (cond
          ;; Terminate message - return set of threads which are still active
          [(equal? next-evt 'terminate) job-threads]
          ;; Job-count message - reply with number of active job threads
          [(equal? next-evt 'job-count)
           (place-channel-put (worker-channel) (set-count job-threads))
           (loop job-threads)]
          ;; Thread terminated - remove it from our list
          [(set-member? job-threads next-evt)
           (loop (set-remove job-threads next-evt))]
          ;; Heavy crunch - demo operation
          [(equal? next-evt 'heavy-crunch)
           (let ([thd (start-heavy-crunch)])
             (loop (set-add job-threads thd)))]
          ;; Unknown event type - log and ignore
          [else
           (worker-log "Received event (~A), don't know what to do with it - discarding." next-evt)
           (loop job-threads)])))
    ;; TODO - clean up remaining threads
    ;; Log shutdown
    (worker-log "Worker terminated."))

  (define (wait-for-job)
    (sync (worker-channel)))

  (define (start-heavy-crunch)
    (thread (λ ()
              (worker-log "heavy crunch whoa!")
              (let loop ([n (random 4294967087)])
                (when (not (zero? n))
                  (loop (sub1 n))))
              (worker-log "that was hard"))))

  (define (worker-log fmt . v)
    (apply rackmount-log (worker-identifier) fmt v)))
