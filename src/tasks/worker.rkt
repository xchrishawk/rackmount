;;
;; worker.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This procedure defines a worker "place". A place is a separate instance of the
;; Racket runtime which is used to support SMP parallelism throughout the server.
;;

#lang racket

;; -- Requires --

(require "../tasks/place.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Creates a new worker.
  ;; - identifier: the identifier of the worker.
  ;; - output-channel: a channel to use to send messages to the manager.
  [rename make-worker worker (->i ([identifier place-identifier?]
                                   [place-to-manager-channel place-channel?])
                                  [result worker?])]

  ;; Predicate returning #t if the argument is a worker object.
  [worker? (-> any/c boolean?)]

  ;; Returns the identifier for the specified worker.
  [worker-identifier (-> worker? place-identifier?)]

  ;; Terminates a worker. If synchronous is #t, this method will block until
  ;; (worker-terminated-evt) is ready for synchronization.
  [worker-terminate (->i ([worker worker?])
                         (#:synchronous [synchronous boolean?])
                         [result void?])]

  ;; Returns an event which is ready for synchronization when the place for the
  ;; specified worker is no longer running.
  [worker-terminated-evt (-> worker? evt?)]))

;; -- Structs --

(struct worker (identifier place))

;; -- Public Procedures --

(define (make-worker identifier _pch-to-manager)
  ;; Create the place
  (let ([pl (place _pch-from-manager
              ;; Set place configuration
              (parameterize*
                  ([pch-from-manager _pch-from-manager]
                   [pch-to-manager (place-channel-get (pch-from-manager))]
                   [place-identifier (place-channel-get (pch-from-manager))])
                ;; Run the main worker procedure
                (worker-main)))])
    ;; Notify place of identifier and channel.
    (place-channel-put pl _pch-to-manager)
    (place-channel-put pl identifier)
    ;; Return struct
    (worker identifier pl)))

(define (worker-put worker message)
  (place-channel-put (worker-place worker) message))

(define (worker-terminate worker #:synchronous [synchronous #t])
  (worker-send worker 'terminate)
  (when synchronous
    (sync (worker-terminated-evt worker)))
  (void))

(define (worker-terminated-evt worker)
  (wrap-evt
   (place-dead-evt (worker-place worker))
   (λ (evt) worker)))

;; -- Private Procedures --

(define (worker-main)
  (displayln (format "TEMP: Worker (~A) started." (place-identifier)))
  (let loop ()
    (sync
     (handle-evt
      (pch-from-manager)
      (match-lambda
        ['terminate (void)]
        [unrecognized-message
         (displayln (format "I am ~A and I got: ~A" (place-identifier) unrecognized-message))
         (loop)]))))
  (displayln (format "TEMP: Worker (~A) terminated." (place-identifier))))
