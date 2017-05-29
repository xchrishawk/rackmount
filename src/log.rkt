;;
;; log.rkt
;; Chris Vig (chris@invictus.so)
;;
;; Module containing utility functions for logging.
;;

#lang racket

;; -- Requires --

(require racket/date)
(require (for-syntax racket/syntax))
(require (for-syntax "utility.rkt"))

;; -- Provides --

(provide

 ;; Macro to define a set of local logging functions.
 define-local-log

 (contract-out

  ;; Parameter defining the current logging level for the server.
  [rackmount-log-level (parameter/c rackmount-log-level?)]

  ;; Predicate returning #t if the specified value is a valid log level.
  [rackmount-log-level? (-> any/c boolean?)]

  ;; Logs an event to the main server log.
  [rackmount-log (->* (rackmount-log-level? string? (or/c string? false?) string?)
                      #:rest (listof any/c)
                      void?)]))

;; -- Constants --

;; Error levels
(define log-levels (hash 0 "Critical"
                         1 "Error"
                         2 "Warning"
                         3 "Info"
                         4 "Debug"
                         5 "Trace"))

;; -- Parameters --

(define rackmount-log-level
  (make-parameter 5))

;; -- Public Procedures --

(define (rackmount-log-level? x)
  (if (hash-ref log-levels x #f) #t #f))

(define (rackmount-log level category identifier fmt . v)
  (when (<= level (rackmount-log-level))
    (let* ([date-string (date-string)]
           [level-string (hash-ref log-levels level)]
           [header-string (if identifier
                              (format "~A / ~A / ~A / ~A" date-string level-string category identifier)
                              (format "~A / ~A / ~A" date-string level-string category))]
           [event-string (apply format fmt v)])
      (displayln (format "[~A]\n~A\n" header-string event-string)))))

;; -- Macros --

(define-syntax (define-local-log stx)
  (syntax-case stx ()
    [(_ name category)
     (let ([name-string (symbol->string (syntax->datum #'name))])
       (with-syntax ([fmt (generate-temporary "fmt")]
                     [v (generate-temporary "v")]
                     [fn-log-critical (make-syntax stx "~A-log-critical" name-string)]
                     [fn-log-error (make-syntax stx "~A-log-error" name-string)]
                     [fn-log-warning (make-syntax stx "~A-log-warning" name-string)]
                     [fn-log-info (make-syntax stx "~A-log-info" name-string)]
                     [fn-log-debug (make-syntax stx "~A-log-debug" name-string)]
                     [fn-log-trace (make-syntax stx "~A-log-trace" name-string)])
         (datum->syntax
          stx
          `(begin
             (define (,#'fn-log-critical ,#'fmt . ,#'v)
               (apply rackmount-log 0 ,#'category #f ,#'fmt ,#'v))
             (define (,#'fn-log-error ,#'fmt . ,#'v)
               (apply rackmount-log 1 ,#'category #f ,#'fmt ,#'v))
             (define (,#'fn-log-warning ,#'fmt . ,#'v)
               (apply rackmount-log 2 ,#'category #f ,#'fmt ,#'v))
             (define (,#'fn-log-info ,#'fmt . ,#'v)
               (apply rackmount-log 3 ,#'category #f ,#'fmt ,#'v))
             (define (,#'fn-log-debug ,#'fmt . ,#'v)
               (apply rackmount-log 4 ,#'category #f ,#'fmt ,#'v))
             (define (,#'fn-log-trace ,#'fmt . ,#'v)
               (apply rackmount-log 5 ,#'category #f ,#'fmt ,#'v))))))]
    [(_ name category #:with-identifier)
     (let ([name-string (symbol->string (syntax->datum #'name))])
       (with-syntax ([identifier (generate-temporary "identifier")]
                     [fmt (generate-temporary "fmt")]
                     [v (generate-temporary "v")]
                     [fn-log-critical (make-syntax stx "~A-log-critical" name-string)]
                     [fn-log-error (make-syntax stx "~A-log-error" name-string)]
                     [fn-log-warning (make-syntax stx "~A-log-warning" name-string)]
                     [fn-log-info (make-syntax stx "~A-log-info" name-string)]
                     [fn-log-debug (make-syntax stx "~A-log-debug" name-string)]
                     [fn-log-trace (make-syntax stx "~A-log-trace" name-string)])
         (datum->syntax
          stx
          `(begin
             (define (,#'fn-log-critical ,#'identifier ,#'fmt . ,#'v)
               (apply rackmount-log 0 ,#'category ,#'identifier ,#'fmt ,#'v))
             (define (,#'fn-log-error ,#'identifier ,#'fmt . ,#'v)
               (apply rackmount-log 1 ,#'category ,#'identifier ,#'fmt ,#'v))
             (define (,#'fn-log-warning ,#'identifier ,#'fmt . ,#'v)
               (apply rackmount-log 2 ,#'category ,#'identifier ,#'fmt ,#'v))
             (define (,#'fn-log-info ,#'identifier ,#'fmt . ,#'v)
               (apply rackmount-log 3 ,#'category ,#'identifier ,#'fmt ,#'v))
             (define (,#'fn-log-debug ,#'identifier ,#'fmt . ,#'v)
               (apply rackmount-log 4 ,#'category ,#'identifier ,#'fmt ,#'v))
             (define (,#'fn-log-trace ,#'identifier ,#'fmt . ,#'v)
               (apply rackmount-log 5 ,#'category ,#'identifier ,#'fmt ,#'v))))))]))

;; -- Private Procedures --

;; Returns a formatted string for the current date.
(define (date-string [date #f])
  (define (padded-number-string value width)
    (~a value #:width width #:align 'right #:pad-string "0"))
  (let* ([date (or date (current-date))])
    (format "~A-~A-~A ~A:~A:~A.~A"
            (padded-number-string (date-year date) 4)
            (padded-number-string (date-month date) 2)
            (padded-number-string (date-day date) 2)
            (padded-number-string (date-hour date) 2)
            (padded-number-string (date-minute date) 2)
            (padded-number-string (date-second date) 2)
            (padded-number-string (exact-floor (/ (date*-nanosecond date) 1000000.0)) 3))))
