#lang racket

(define (reduce func ls ntimes)
  (cond
   [(cons? ls) (func (car ls) ntimes) (reduce func (cdr ls) ntimes) ]))

(define (func a ntimes)
    (cond
     [(> ntimes 0) (printf "~a" (string-append (number->string a) "\n")) (func a (- ntimes 1))]))

(define (f n arr)
  (reduce func n arr))

(define (read-list)
  (let ([x (read)]) 
    (if (eof-object? x)
        (list)
        (cons (number->string x) (read-list)))))

(define t (read))
(define xs (read-list))

(f t xs)