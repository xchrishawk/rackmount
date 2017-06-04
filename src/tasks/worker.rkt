;;
;; worker.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This procedure defines a worker "place". A place is a separate instance of the
;; Racket runtime which is used to support SMP parallelism throughout the server.
;;

#lang racket

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

  ;; Terminates a worker. If synchronous is #t, this method will block until
  ;; (worker-terminated-evt) is ready for synchronization.
  [worker-terminate (->i ([worker worker?])
                         (#:synchronous [synchronous boolean?])
                         [result void?])]

  ;; Returns an event which is ready for synchronization when the place for the
  ;; specified worker is no longer running.
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

(define (worker-put worker message)
  (place-channel-put (worker-place worker) message))

(define (worker-terminate worker #:synchronous [synchronous #t])
  (worker-put worker 'terminate)
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

(define (worker-main)
  (displayln (format "TEMP: Worker (~A) started." (current-worker-identifier)))
  (let loop ()
    (sync
     (handle-evt
      (current-worker-pch-from-manager)
      (match-lambda
        ['terminate (void)]
        [unrecognized-message
         (displayln (format "I am ~A and I got: ~A" (current-worker-identifier) unrecognized-message))
         (loop)]))))
  (displayln (format "TEMP: Worker (~A) terminated." (current-worker-identifier))))

;; -- Tests --

(module+ test

  ;; -- Requires --

  (require rackunit)

  ;; -- Helpers --

  (define (worker-and-pch [identifier "Example"])
    (let*-values ([(our-pch their-pch) (place-channel)]
                  [(worker) (make-worker identifier their-pch)])
      (values worker our-pch)))

  ;; -- Test Cases --

  (test-case "Lifecycle"
    (let-values ([(worker pch) (worker-and-pch)])
      (check-false (sync/timeout 1.0 (worker-terminated-evt worker)))
      (worker-terminate worker #:synchronous #f)
      (check-equal? (sync/timeout 5.0 (worker-terminated-evt worker)) worker))))
