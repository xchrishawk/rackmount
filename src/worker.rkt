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

  ;; -- Worker Management --

  ;; Create a new worker with the specified identifier.
  [make-worker (-> string? opaque-worker?)]
  ;; Returns the identifier of a worker.
  [worker-identifier (-> opaque-worker? string?)]
  ;; Sends a job to a worker.
  [worker-send-job (-> opaque-worker? any/c void?)]
  ;; Returns the number of active jobs the worker has.
  [worker-job-count (-> opaque-worker? exact-nonnegative-integer?)]
  ;; Terminates the specified worker.
  [worker-terminate (->* (opaque-worker?)
                         (#:finish-jobs boolean?
                          #:synchronous boolean?)
                         void?)]
  ;; Returns a syncable event which is ready after the specified worker is terminated.
  [worker-terminated-evt (-> opaque-worker? evt?)]

  ;; -- Worker Groups --

  ;; Makes N workers with auto-generated identifiers.
  [make-workers (-> exact-positive-integer? (listof opaque-worker?))]
  ;; Sends a job to the worker with the lowest number of active jobs.
  [workers-send-job (-> (listof opaque-worker?) any/c void?)]
  ;; Terminates all of the specified workers.
  [workers-terminate (->* ((listof opaque-worker?))
                          (#:finish-jobs boolean?
                           #:synchronous boolean?)
                          void?)]))

;; -- Structs --

;; Opaque struct containing worker data.
(struct opaque-worker (identifier place channel))

;; -- Public Procedures (Worker Management) --

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

(define (worker-terminate worker
                          #:finish-jobs [finish-jobs #t]
                          #:synchronous [synchronous #t])
  (place-channel-put (opaque-worker-channel worker)
                     (if finish-jobs 'terminate-after-completion 'terminate-immediately))
  (when synchronous
    (sync (worker-terminated-evt worker)))
  (void))

(define (worker-terminated-evt worker)
  (place-dead-evt (opaque-worker-place worker)))

;; -- Public Procedures (Worker Groups) --

(define (make-workers count)
  (for/list ([n (in-range count)])
    (let ([identifier (format "Worker ~A" n)])
      (make-worker identifier))))

(define (workers-send-job workers job)
  (let ([worker (worker-with-lowest-job-count workers)])
    (worker-send-job worker job)))

(define (workers-terminate workers
                           #:finish-jobs [finish-jobs #t]
                           #:synchronous [synchronous #t])
  ;; Send terminate to all workers immediately
  (for ([worker (in-list workers)])
    (worker-terminate worker #:finish-jobs finish-jobs #:synchronous #f))
  ;; Wait for them all to complete
  (when synchronous
    (for ([worker (in-list workers)])
      (sync (worker-terminated-evt worker)))))

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
    (let ([remaining-threads
           (let loop ([job-threads (set)] [terminating #f])
             ;; Wait for an event to occur
             (let* ([syncable-evts (list (worker-channel)		; message from controller
                                         (set->list job-threads))]     	; job thread termination
                    [next-evt (apply sync (flatten syncable-evts))])
               (cond

                 ;; 'terminate-immediately message - immediately quit and return set of threads
                 [(equal? next-evt 'terminate-immediately)
                  (when (not (set-empty? job-threads))
                    (worker-log "Warning: terminating while there are still ~A jobs active!"
                                (set-count job-threads)))
                  job-threads]

                 ;; 'terminate-after-completion message - quit after all jobs complete
                 [(equal? next-evt 'terminate-after-completion)
                  (cond
                    ;; No active jobs, OK to shut down immediately
                    [(set-empty? job-threads) job-threads]
                    ;; At least one active job, need to wait for them to terminate
                    [else
                     (worker-log "Terminating after ~A jobs complete..."
                                 (set-count job-threads))
                     (loop job-threads #t)])]

                 ;; Job-count message - reply with number of active job threads
                 [(equal? next-evt 'job-count)
                  (place-channel-put (worker-channel) (set-count job-threads))
                  (loop job-threads terminating)]

                 ;; Thread terminated - remove it from our list
                 [(set-member? job-threads next-evt)
                  (let ([new-job-threads (set-remove job-threads next-evt)])
                    (cond
                      ;; We are terminating *and* our last job just finished. Close the worker.
                      [(and terminating (set-empty? new-job-threads))
                       new-job-threads]
                      ;; Either we're not terminating or there are still jobs left. Keep going.
                      [else
                       (loop new-job-threads terminating)]))]

                 ;; Demo operation
                 [(equal? next-evt 'heavy-crunch)
                  (let ([thd (start-heavy-crunch)])
                    (loop (set-add job-threads thd) terminating))]

                 ;; Unknown event type - log and ignore
                 [else
                  (worker-log "Received event (~A), don't know what to do with it - discarding." next-evt)
                  (loop job-threads terminating)])))])

      ;; Immediately kill any remaining threads
      (when (not (set-empty? remaining-threads))
        (worker-log "Immediately terminating ~A jobs." (set-count remaining-threads))
        (for ([thd (in-set remaining-threads)])
          (kill-thread thd)))

      ;; Log shutdown
      (worker-log "Worker terminated.")))

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
