;;
;; version.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module contains program version information.
;;

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; The name of the server.
  [rackmount-name string?]

  ;; The major version number.
  [rackmount-major-version exact-nonnegative-integer?]

  ;; The minor version number.
  [rackmount-minor-version exact-nonnegative-integer?]

  ;; The revision version number.
  [rackmount-revision-version exact-nonnegative-integer?]

  ;; Returns a formatted string representing the current server version.
  [rackmount-version (-> string?)]))

;; -- Objects --

(define rackmount-name "Rackmount")
(define rackmount-major-version 0)
(define rackmount-minor-version 1)
(define rackmount-revision-version 0)

;; -- Public Procedures --

(define rackmount-version
  (const
   (format
    "~A ~A.~A.~A"
    rackmount-name
    rackmount-major-version
    rackmount-minor-version
    rackmount-revision-version)))
