#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (b n)
    (cond
    [(> n 0) (display "Hello World\n") (b (- n 1))]))

(let ([a (read)])
    (b a))