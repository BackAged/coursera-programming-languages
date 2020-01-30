#lang racket

(define (read-list)
  (let ([x (read)]) 
    (if (eof-object? x)
        (list)
        (cons  x (read-list)))))

(define (filter ls fn indx)
    (cond
    [(empty? ls) empty]
    [(fn indx) (cons (first ls) (filter (rest ls) fn (+ indx 1)))]
    [else (filter (rest ls) fn (+ indx 1))]))

(let* ([ls (read-list)]
    [filtered (filter ls (lambda (indx) (= (modulo indx 2) 0)) 1)])
    (for ([x filtered]) (printf "~a\n" x)))