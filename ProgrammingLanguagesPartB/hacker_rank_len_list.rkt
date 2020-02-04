#lang racket
(define (read-list)
  (let ([r (read)])
    (if (eof-object? r) empty
        (cons r (read-list)))))

(define (reduce fn ls acc)
  (cond
    [(empty? ls) acc]
    [else (reduce fn (cdr ls) (fn (car ls) acc))]))
  

(let* ([ls (read-list)]
  [length (reduce (lambda (cur acc) (+ acc 1)) ls 0)])
  (print length))