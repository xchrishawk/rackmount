;;
;; session-task.rkt
;; Chris Vig (chris@invictus.so)
;;
;; This module defines the session task handle and task objects, which are used to
;; coordinate execution of session handling procedures on worker threads.
;;

#lang racket

;; -- Requires --

(require "../server/session.rkt")
(require "../tasks/task.rkt")
(require "../util/logging.rkt")
(require "../util/misc.rkt")

;; -- Provides --

(provide
 (contract-out

  ;; Task handle struct for session tasks.
  [struct session-task-handle ([identifier gen:task-identifier?]
                               [input-port input-port?]
                               [output-port output-port?]
                               [working-dir path-string?])]))

;; -- Types --

(struct session-task-handle (identifier
                             input-port
                             output-port
                             working-dir)
  #:transparent
  #:methods gen:task-handle
  [(define (gen:task-handle-identifier task-handle)
     (session-task-handle-identifier task-handle))

   (define (gen:task-handle-initialize task-handle)
     (session-task-log-trace
      (session-task-handle-identifier task-handle)
      "Session task initialized."))

   (define (gen:task-handle-close task-handle)
     (close-input-port (session-task-handle-input-port task-handle))
     (close-output-port (session-task-handle-output-port task-handle))
     (session-task-log-trace
      (session-task-handle-identifier task-handle)
      "Session task closed. Socket connection terminated."))

   (define (gen:task-handle->gen:task task-handle)
     (session-task (session-task-handle-identifier task-handle)
                   (session-task-handle-input-port task-handle)
                   (session-task-handle-output-port task-handle)
                   (session-task-handle-working-dir task-handle)))])

(struct session-task (identifier
                      input-port
                      output-port
                      working-dir
                      [thread #:auto #:mutable])
  #:auto-value #f
  #:methods gen:task
  [(define (gen:task-identifier task)
     (session-task-identifier task))

   (define (gen:task-start task)
     (let ([thd (thread-start
                 (session-proc
                  (session-task-identifier task)
                  (session-task-input-port task)
                  (session-task-output-port task)
                  (session-task-working-dir task)))])
       (set-session-task-thread! task thd)))

   (define (gen:task-cancel task #:synchronous [synchronous #t])
     (let ([thd (session-task-thread task)])
       (when (and thd (thread-running? thd))
         (thread-send thd 'terminate)
         (when synchronous
           (sync thd))))
     (void))

   (define (gen:task-completed-evt task)
     (let ([thd (session-task-thread task)])
       (if thd
           (wrap-evt thd (thunk* task))
           never-evt)))])

;; -- Private Procedures --

(define-local-log session-task "Session Task" #:require-identifier #t)
