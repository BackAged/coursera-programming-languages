#lang racket

(define (f n)
  (cond
  [(zero? n) empty]
  [else (cons n (f (- n 1)))]))

(f 4)