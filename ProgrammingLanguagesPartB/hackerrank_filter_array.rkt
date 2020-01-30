#lang racket
; Enter your code here. Read input from STDIN. Print output to STDOUT

(define (read-list)
  (let ([x (read)]) 
    (if (eof-object? x)
        (list)
        (cons  x (read-list)))))

(define (filter ls fn)
    (cond
    [(empty? ls) null]
    [else (if (fn (car ls)) (cons (car ls)  (filter (cdr ls) fn)) (filter (cdr ls) fn))]))


(let* ([delimeter (read)]
    [ls (read-list)]
    [filtered (filter ls (lambda (x) (< x delimeter)))])
    (map (lambda (x) (printf "~a\n" x)) filtered))