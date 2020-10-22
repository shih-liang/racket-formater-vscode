#lang racket/base
(require racket/class
         framework)

(define t (new racket:text%))
(define (indent port)
  (send t insert-port port)
  (send t tabify-all)
  (send t get-text))

((lambda (s) (display "succeed|") (display s))
 (call-with-exception-handler
  (lambda ex (display "failed|") (display ex) (exit 1))
  (lambda () (indent (current-input-port)))))