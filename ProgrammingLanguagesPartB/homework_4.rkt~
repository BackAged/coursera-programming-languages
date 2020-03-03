#lang racket

(define (sequence low high stride)
  (cond
    [(> low high) null]
    [else (cons low (sequence (+ low stride) high stride))]))


(define (string-append-map xs suffix)
  (map (lambda (el) (string-append suffix el)) xs))

(define (list-nth-mod xs n)
  xs)

(define (stream-for-n-steps s n)
  (cond
    [(or (zero? n) (negative? n)) null]
    [else
     (let ([p (s)])
       (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))])) 

(define (funny-number-stream)
  (letrec ([fdn (lambda (x) (if (zero? (modulo x 5)) (* -1 x) x ))]
           [f (lambda (x) (cons (fdn x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

