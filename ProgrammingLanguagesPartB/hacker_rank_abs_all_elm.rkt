#lang racket

(define (map fn ls)
  (cond
    [(empty? ls) empty]
    [else (cons (fn (car ls)) (map fn (cdr ls)))]))

(define (f lst)
  (map (lambda (x) (abs x)) lst))

(define (read-list)
  (let ([x (read)]) 
    (if (eof-object? x)
      (list)
      (cons x (read-list)))))

(let ([lst (read-list)]) 
  (let ([ans (f lst)])
    (for ([x ans])
         (printf "~a\n" x))))