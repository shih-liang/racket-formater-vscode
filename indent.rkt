#lang racket/base
(require racket/class
         framework)

(define t (new racket:text%))
(define (indent port)
  (send t insert-port port)
  (send t tabify-all)
  (send t get-text))

((call-with-current-continuation
  (lambda (k)
    (call-with-exception-handler
     (lambda (ex) (k (lambda () (display "failed|") (display ex) (newline))))
     (lambda ()
       (let ([rs (indent (current-input-port))])
         (lambda () (display "succeed|") (display rs))))))))
