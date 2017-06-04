;;
;; example-task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines an example task for testing purposes.
;;

#lang racket

;; -- Requires --

(require "../tasks/task.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Task handle struct for the example task.
  [struct example-task-handle ([identifier gen:task-identifier?]
                               [message string?]
                               [loop-until-cancelled boolean?])]))

;; -- Types --

(struct example-task-handle (identifier
                             message
                             loop-until-cancelled)
  #:transparent
  #:methods gen:task-handle
  [(define (gen:task-handle-identifier task-handle)
     (example-task-handle-identifier task-handle))
   (define (gen:task-handle-initialize task-handle)
     (displayln
      (format "Initializing task ~A!" (example-task-handle-identifier task-handle))))
   (define (gen:task-handle-close task-handle)
     (displayln
      (format "Closing task ~A!" (example-task-handle-identifier task-handle))))
   (define (gen:task-handle->gen:task task-handle)
     (example-task
      (example-task-handle-identifier task-handle)
      (example-task-handle-message task-handle)
      (example-task-handle-loop-until-cancelled task-handle)))])

(struct example-task (identifier
                      message
                      loop-until-cancelled
                      [thread #:auto #:mutable])
  #:auto-value #f
  #:methods gen:task
  [(define (gen:task-identifier task)
     (example-task-identifier task))
   (define (gen:task-start task)
     (let* ([text (format
                   "~A says ~A"
                   (example-task-identifier task)
                   (example-task-message task))]
            [loop-until-cancelled (example-task-loop-until-cancelled task)]
            [thd (thread (thunk
                          (let loop ()
                            (displayln text)
                            (when (and
                                   loop-until-cancelled
                                   (not (sync/timeout 1.0 (thread-receive-evt))))
                              (loop)))))])
       (set-example-task-thread! task thd)))
   (define (gen:task-cancel task #:synchronous [synchronous #t])
     (let ([thd (example-task-thread task)])
       (when thd
         (thread-send thd 'terminate)
         (when synchronous
           (sync thd))))
     (void))
   (define (gen:task-completed-evt task)
     (let ([thd (example-task-thread task)])
       (if thd
           (wrap-evt thd (λ (evt) task))
           never-evt)))])
