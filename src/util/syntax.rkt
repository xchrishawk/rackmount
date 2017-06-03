;;
;; syntax.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines helpers for macros.
;;

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; Creates a syntax object for a symbol using the specified format string.
  [make-syntax/symbol (->* (syntax? string?) #:rest (listof any/c) syntax?)]))

;; -- Public Procedures --

(define (make-syntax/symbol stx fmt . v)
  (datum->syntax stx (string->symbol (apply format fmt v))))
