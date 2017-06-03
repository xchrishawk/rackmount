;;
;; place.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines configuration and utility methods for the current place.
;;

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; The identifier of the currently active place.
  [place-identifier (parameter/c place-identifier? (or/c place-identifier? false?))]

  ;; The place channel to use to send messages *to* the manager.
  [pch-to-manager (parameter/c place-channel? (or/c place-channel? false?))]

  ;; The place channel to use to receive messages *from* the manager.
  [pch-from-manager (parameter/c place-channel? (or/c place-channel? false?))]

  ;; Predicate returning #t if the argument is a valid place identifier.
  [place-identifier? (-> any/c boolean?)]))

;; -- Parameters --

(define place-identifier
  (make-parameter #f))

(define pch-to-manager
  (make-parameter #f))

(define pch-from-manager
  (make-parameter #f))

;; -- Public Procedures --

(define place-identifier?
  string?)
