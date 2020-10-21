#lang racket/base
(require racket/class
         framework)

(define t (new racket:text%))
(define (indent port)
  (send t insert-port port)
  (send t tabify-all)
  (send t get-text))

(display (indent (current-input-port)))
