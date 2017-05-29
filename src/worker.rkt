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
