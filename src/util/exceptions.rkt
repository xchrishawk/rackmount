;;
;; exceptions.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Module defining custom exception types.
;;

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; Exception struct representing an error thrown by an unrecognized thread message.
  [struct exn:fail:rackmount:bad-message ([message string?]
                                          [continuation-marks continuation-mark-set?])]

  ;; Raises an exn:fail:rackmount:bad-message error for the specified message.
  [raise-bad-message-error (-> any/c any)]))

;; -- Custom Exceptions --

(struct exn:fail:rackmount:bad-message exn:fail ()
  #:extra-constructor-name make-exn:fail:rackmount:bad-message
  #:transparent)

(define (raise-bad-message-error message)
  (raise
   (make-exn:fail:rackmount:bad-message
    (format "Received unknown thread or place message! ~A" message)
    (current-continuation-marks))))
