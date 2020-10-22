#lang racket
(require racket/pretty)

(define (pretty-write-all in-port out-port)
  (cond
    ((eof-object? (peek-char in-port)) #t)
    ((eq? #\# (peek-char in-port))
     (write-string (read-line in-port 'any) out-port)
     (newline out-port)
     (pretty-write-all in-port out-port))
    ((memq (peek-char in-port) '(#\return #\newline))
     (write-char (read-char in-port) out-port)
     (pretty-write-all in-port out-port))
    (else
     (let ((next (read in-port)))
       (when (not (eof-object? next))
         (pretty-write next out-port #:newline? #f)
         (pretty-write-all in-port out-port))))))

((call-with-current-continuation
  (lambda (k)
    (call-with-exception-handler
     (lambda (ex) (k (lambda () (display "failed|") (display ex) (newline))))
     (lambda ()
       (let ((op (open-output-string)))
         (pretty-write-all (current-input-port) op)
         (lambda () (display "succeed|") (display (get-output-string op)))))))))
